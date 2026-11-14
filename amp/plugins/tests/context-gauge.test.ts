import { afterEach, expect, jest, test } from 'bun:test'
import { copyFileSync, mkdirSync, mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { resolve } from 'node:path'
import { compactedSince, defaults, linesFrom, readingsFrom, step } from '../context-gauge/index.ts'

// No real plugin host or Amp connection is used by this suite.
const cleanups: (() => void)[] = []
afterEach(() => { jest.useRealTimers(); for (const cleanup of cleanups.splice(0)) cleanup() })
const flush = () => new Promise(resolve => setImmediate(resolve))
let messageCount = 0
const assistant = (cacheRead: number | null, cacheCreation: number | null = 0, input = 2) =>
  ({role: 'assistant', id: `M-${++messageCount}`, content: [], usage: {inputTokens: input, outputTokens: 999_999, cacheReadInputTokens: cacheRead, cacheCreationInputTokens: cacheCreation}}) as any
const lines = {warnTokens: 300_000, landTokens: 380_000}

async function setup() {
  // The plugin logs beside its own checkout, so run a copy to keep the real log clean.
  const root = mkdtempSync(resolve(tmpdir(), 'context-gauge-'))
  const dir = resolve(root, '.agents/bin/context-gauge')
  mkdirSync(dir, {recursive: true})
  copyFileSync(resolve(import.meta.dir, '../context-gauge/index.ts'), resolve(dir, 'index.ts'))
  cleanups.push(() => rmSync(root, {recursive: true, force: true}))
  const state = {
    transcript: {} as Record<string, any[]>, threadState: 'running', config: {} as Record<string, unknown>, failAppend: false, failRead: false,
    // Tests hold a read or an append open by putting a promise here.
    readGate: undefined as Promise<void> | undefined, appendGate: undefined as Promise<void> | undefined,
    appends: [] as {target: string, content: string, options: unknown}[], toasts: [] as string[],
    handlers: {} as Record<string, (event: any) => unknown>, commands: {} as Record<string, (ctx: any) => Promise<void>>,
  }
  const thread = (id: string) => ({
    id, state: {async get() { return state.threadState }},
    // Like the real host: only the full transcript carries usage.
    async messages(options: {full?: boolean, limit: number}) {
      if (state.failRead) throw new Error('read failed')
      // What the transcript held when the read began, however long the read takes.
      const messages = (state.transcript[id] ?? []).slice(-options.limit)
      await state.readGate
      return options.full ? messages : messages.map(({usage, ...rest}) => rest)
    },
    async appendUserMessage(message: {content: string}, options: unknown) {
      if (state.failAppend) throw new Error('append failed')
      await state.appendGate
      state.appends.push({target: id, content: message.content, options})
    },
  })
  const api = {
    logger: {log() {}}, threads: {get: thread}, configuration: {async get() { return state.config }},
    ui: {async notify(message: string) { state.toasts.push(message) }},
    on(event: string, handler: (event: any) => unknown) { state.handlers[event] = handler },
    registerCommand(id: string, _options: unknown, handler: (ctx: any) => Promise<void>) { state.commands[id] = handler },
  }
  ;(await import(resolve(dir, 'index.ts'))).default(api as any)
  const emit = (id: string, tokens: number) => {
    // Each tool result follows a new request.
    state.transcript[id] = [...(state.transcript[id] ?? []), assistant(tokens - 2)]
    // The handler must not hold back or replace the tool result.
    expect(state.handlers['tool.result']({thread: {id}, tool: 'shell_command', status: 'done'})).toBeUndefined()
  }
  const toolResult = async (id: string, tokens: number) => { emit(id, tokens); await flush() }
  const logged = () => {
    try { return readFileSync(resolve(root, '.amp/in/context-gauge.jsonl'), 'utf8').trim().split('\n').map(line => JSON.parse(line)) }
    catch { return [] }
  }
  return {state, emit, toolResult, logged, thread}
}
const gate = () => { let open!: () => void; return {closed: new Promise<void>(resolve => { open = resolve }), open: () => open()} }

const tokensOf = (messages: any[]) => readingsFrom(messages).map(reading => reading.tokens)

test('readingsFrom adds fresh, cache-read and cache-creation input of each request, oldest first', () => {
  expect(tokensOf([assistant(100_000, 50), assistant(193_834, 1_480)])).toEqual([100_052, 195_316])
  expect(tokensOf([assistant(null, null, 7_000)])).toEqual([7_000])
})

test('readingsFrom skips messages without usage and keeps the message ID', () => {
  const user = {role: 'user', id: 'M-user', content: []} as any
  const unrecorded = {role: 'assistant', id: 'M-unrecorded', content: []} as any
  const recorded = assistant(41_000)
  expect(readingsFrom([recorded, unrecorded, user])).toEqual([{id: recorded.id, tokens: 41_002}])
  expect(readingsFrom([user, unrecorded])).toEqual([])
})

test('step notifies each line once, at the line itself, and keeps quiet in between', () => {
  expect(step(0, 299_999, lines)).toEqual({notified: 0})
  expect(step(0, 300_000, lines)).toEqual({notified: 1, notice: 'warn'})
  expect(step(1, 379_999, lines)).toEqual({notified: 1})
  expect(step(1, 380_000, lines)).toEqual({notified: 2, notice: 'land'})
  expect(step(2, 500_000, lines)).toEqual({notified: 2})
  // A thread that first reports past the land line gets the land notice only.
  expect(step(0, 400_000, lines)).toEqual({notified: 2, notice: 'land'})
  // Lines raised above an announced level do not take the announcement back.
  expect(step(1, 310_000, {warnTokens: 500_000, landTokens: 600_000})).toEqual({notified: 1})
})

test('compactedSince sees a fall between any two consecutive readings after the mark', () => {
  const reading = (id: string, tokens: number) => ({id, tokens})
  const mark = reading('a', 430_000)
  // The marked reading itself, growth, and small dips are the same window.
  expect(compactedSince(mark, [reading('a', 430_000)])).toBe(false)
  expect(compactedSince(mark, [reading('a', 430_000), reading('b', 344_000), reading('c', 436_000)])).toBe(false)
  expect(compactedSince(mark, [reading('a', 430_000), reading('b', 343_999)])).toBe(true)
  // The context fell and grew back before the gauge looked again.
  expect(compactedSince(reading('a', 178_000), [reading('a', 178_000), reading('b', 85_000), reading('c', 150_000)])).toBe(true)
  // Readings before the mark are another window's business.
  expect(compactedSince(reading('c', 150_000), [reading('a', 178_000), reading('b', 85_000), reading('c', 150_000)])).toBe(false)
  // A mark that has scrolled out of the readings is compared with all of them.
  expect(compactedSince(mark, [reading('x', 90_000), reading('y', 95_000)])).toBe(true)
  expect(compactedSince(mark, [reading('x', 432_000), reading('y', 435_000)])).toBe(false)
})

test('linesFrom reads Amp settings and falls back to the defaults as a pair', () => {
  expect(linesFrom({})).toEqual(defaults)
  expect(linesFrom({'contextGauge.warnTokens': 150_000, 'contextGauge.landTokens': 170_000})).toEqual({warnTokens: 150_000, landTokens: 170_000})
  expect(linesFrom({'contextGauge.warnTokens': 100_000})).toEqual({warnTokens: 100_000, landTokens: defaults.landTokens})
  expect(linesFrom({'contextGauge.landTokens': 200_000})).toEqual(defaults)
  expect(linesFrom({'contextGauge.warnTokens': '100000', 'contextGauge.landTokens': 200_000})).toEqual(defaults)
  expect(linesFrom({'contextGauge.warnTokens': 0, 'contextGauge.landTokens': 200_000})).toEqual(defaults)
})

test('a thread gets one steering notice per line and both again after a compaction', async () => {
  const {state, toolResult, logged} = await setup()
  const id = 'T-seat'
  await toolResult(id, 299_000)
  expect(state.appends).toEqual([])
  await toolResult(id, 301_234)
  expect(state.appends).toHaveLength(1)
  expect(state.appends[0]).toMatchObject({target: id, options: {steer: true}})
  expect(state.appends[0].content).toContain('301,234 input tokens, past the warn line of 300,000')
  expect(state.appends[0].content).toContain('carry on with any work in progress')
  await toolResult(id, 350_000)
  expect(state.appends).toHaveLength(1)
  await toolResult(id, 381_000)
  expect(state.appends).toHaveLength(2)
  expect(state.appends[1].content).toContain('past the land line of 380,000')
  expect(state.appends[1].content).toContain('ask Peter whether to land the plane now')
  await toolResult(id, 430_000)
  await toolResult(id, 70_000)
  expect(state.appends).toHaveLength(2)
  await toolResult(id, 305_000)
  expect(state.appends).toHaveLength(3)
  expect(logged().map(entry => [entry.event, entry.tokens])).toEqual([['warn', 301_234], ['land', 381_000], ['warn', 305_000]])
  expect(state.toasts).toHaveLength(3)
})

test('parallel tool results of one request send one notice', async () => {
  const {state} = await setup()
  state.transcript['T-seat'] = [assistant(310_000)]
  for (let i = 0; i < 3; i++) state.handlers['tool.result']({thread: {id: 'T-seat'}})
  await flush()
  expect(state.appends).toHaveLength(1)
})

test('a failed append is logged and sent again on the next tool result', async () => {
  const {state, toolResult, logged} = await setup()
  state.failAppend = true
  await toolResult('T-seat', 310_000)
  expect(state.appends).toEqual([])
  expect(logged()).toMatchObject([{event: 'check-failed', thread: 'T-seat'}])
  state.failAppend = false
  await toolResult('T-seat', 311_000)
  expect(state.appends).toHaveLength(1)
})

test('threads are gauged separately and lines follow live settings', async () => {
  const {state, toolResult} = await setup()
  await toolResult('T-a', 310_000)
  await toolResult('T-b', 200_000)
  expect(state.appends.map(append => append.target)).toEqual(['T-a'])
  state.config = {'contextGauge.warnTokens': 150_000, 'contextGauge.landTokens': 250_000}
  await toolResult('T-b', 201_000)
  expect(state.appends.map(append => append.target)).toEqual(['T-a', 'T-b'])
  expect(state.appends[1].content).toContain('past the warn line of 150,000')
})

test('a tool result that arrives during a check gets its own reading afterwards', async () => {
  const {state, emit} = await setup()
  const read = gate()
  state.readGate = read.closed
  emit('T-seat', 299_000)
  await flush()
  // The first read is still open when a newer request reports past the land line.
  emit('T-seat', 381_000)
  await flush()
  state.readGate = undefined
  read.open()
  await flush()
  expect(state.appends.map(append => append.content.includes('381,000 input tokens, past the land line'))).toEqual([true])
})

test('a compaction is remembered when the new window\'s first notice is deferred', async () => {
  const {state, toolResult} = await setup()
  state.config = {'contextGauge.warnTokens': 50_000, 'contextGauge.landTokens': 100_000}
  await toolResult('T-seat', 178_000)
  state.threadState = 'idle'
  await toolResult('T-seat', 85_000)
  state.threadState = 'running'
  await toolResult('T-seat', 150_000)
  expect(state.appends.map(entry => entry.content.includes('warn line of') ? 'warn' : 'land')).toEqual(['land', 'land'])
  expect(state.appends[1].content).toContain('150,000 input tokens')
})

test('a compaction is remembered while the new window\'s notices keep failing', async () => {
  const {state, toolResult} = await setup()
  state.config = {'contextGauge.warnTokens': 50_000, 'contextGauge.landTokens': 100_000}
  await toolResult('T-seat', 178_000)
  state.failAppend = true
  // More failed checks than one read returns, ending close to the old window's last reading.
  for (const tokens of [85_000, 120_000, 143_000, 144_000, 145_000, 146_000, 147_000, 148_000, 149_000, 149_500, 149_800]) await toolResult('T-seat', tokens)
  state.failAppend = false
  await toolResult('T-seat', 150_000)
  expect(state.appends.map(entry => entry.content.match(/sent ([\d,]+) input/)?.[1])).toEqual(['178,000', '150,000'])
})

test('a compaction and regrowth during one slow check still start a new window', async () => {
  const {state, emit} = await setup()
  state.config = {'contextGauge.warnTokens': 50_000, 'contextGauge.landTokens': 100_000}
  const append = gate()
  state.appendGate = append.closed
  emit('T-seat', 178_000)
  await flush()
  for (const tokens of [85_000, 150_000]) { emit('T-seat', tokens); await flush() }
  state.appendGate = undefined
  append.open()
  await flush()
  expect(state.appends.map(entry => entry.content.match(/sent ([\d,]+) input/)?.[1])).toEqual(['178,000', '150,000'])
})

test('a slow append is never sent twice, and later tool results are still read once it settles', async () => {
  const {state, emit} = await setup()
  const append = gate()
  state.appendGate = append.closed
  emit('T-seat', 310_000)
  await flush()
  for (const tokens of [320_000, 330_000, 390_000]) { emit('T-seat', tokens); await flush() }
  expect(state.appends).toEqual([])
  state.appendGate = undefined
  append.open()
  await flush()
  expect(state.appends.map(entry => entry.content.includes('warn line of') ? 'warn' : 'land')).toEqual(['warn', 'land'])
})

test('raising and restoring the lines does not repeat a notice in the same window', async () => {
  const {state, toolResult} = await setup()
  await toolResult('T-seat', 310_000)
  state.config = {'contextGauge.warnTokens': 500_000, 'contextGauge.landTokens': 600_000}
  await toolResult('T-seat', 320_000)
  state.config = {}
  await toolResult('T-seat', 325_000)
  expect(state.appends).toHaveLength(1)
})

test('lowered lines keep working across compactions on a small window', async () => {
  const {state, toolResult} = await setup()
  state.config = {'contextGauge.warnTokens': 100_000, 'contextGauge.landTokens': 150_000}
  for (const tokens of [120_000, 178_000, 85_000, 101_000, 160_000, 93_000, 99_000, 100_000]) await toolResult('T-seat', tokens)
  expect(state.appends.map(entry => entry.content.includes('warn line of') ? 'warn' : 'land')).toEqual(['warn', 'land', 'warn', 'land', 'warn'])
})

test('a notice due after the turn ended waits for the next tool result instead of waking the thread', async () => {
  const {state, toolResult, logged} = await setup()
  state.threadState = 'idle'
  await toolResult('T-seat', 381_000)
  expect(state.appends).toEqual([])
  expect(logged()).toMatchObject([{event: 'deferred', thread: 'T-seat', notice: 'land', state: 'idle'}])
  // Peter still learns of it, although the agent is not told yet.
  expect(state.toasts).toEqual(['Context gauge: thread T-seat passed the land line at 381,000 input tokens while idle; its agent is told at its next tool result.'])
  // Parallel results of the same idle request, and the later delivery, do not announce it again.
  for (let i = 0; i < 3; i++) state.handlers['tool.result']({thread: {id: 'T-seat'}})
  await flush()
  state.threadState = 'awaiting-approval'
  await toolResult('T-seat', 381_500)
  expect(state.toasts).toHaveLength(1)
  expect(state.appends).toHaveLength(1)
  expect(state.appends[0].content).toContain('381,500 input tokens, past the land line')
})

test('a read that never answers is given up after 30 seconds and the next tool result is checked', async () => {
  const {state, emit, logged} = await setup()
  jest.useFakeTimers()
  const settle = async () => { for (let i = 0; i < 20; i++) await Promise.resolve() }
  state.readGate = new Promise(() => {})
  emit('T-seat', 310_000)
  await settle()
  jest.advanceTimersByTime(29_999)
  await settle()
  expect(logged()).toEqual([])
  jest.advanceTimersByTime(1)
  await settle()
  expect(logged()).toMatchObject([{event: 'check-failed', thread: 'T-seat', error: 'Error: No answer from Amp within 30 seconds'}])
  state.readGate = undefined
  emit('T-seat', 311_000)
  await settle()
  expect(state.appends.map(entry => entry.content.match(/sent ([\d,]+) input/)?.[1])).toEqual(['311,000'])
})

test('an append that never answers is not given up, so no second notice can follow it', async () => {
  const {state, emit, logged} = await setup()
  jest.useFakeTimers()
  const settle = async () => { for (let i = 0; i < 20; i++) await Promise.resolve() }
  let attempts = 0
  state.appendGate = {then() { attempts++ }} as any
  emit('T-seat', 310_000)
  await settle()
  jest.advanceTimersByTime(30_000)
  await settle()
  emit('T-seat', 390_000)
  await settle()
  jest.advanceTimersByTime(120_000)
  await settle()
  expect(attempts).toBe(1)
  expect(logged().map(entry => entry.event)).toEqual(['append-slow'])
})

test('the show command reports the reading and lines of the active thread', async () => {
  const {state, thread} = await setup()
  const shown: string[] = []
  const ui = {async notify(message: string) { shown.push(message) }}
  state.transcript['T-seat'] = [assistant(195_314)]
  await state.commands['context-gauge-show']({ui, thread: thread('T-seat')})
  await state.commands['context-gauge-show']({ui, thread: thread('T-empty')})
  await state.commands['context-gauge-show']({ui})
  state.failRead = true
  await state.commands['context-gauge-show']({ui, thread: thread('T-seat')})
  state.failRead = false
  state.readGate = new Promise(() => {})
  jest.useFakeTimers()
  const hung = state.commands['context-gauge-show']({ui, thread: thread('T-seat')})
  for (let i = 0; i < 20; i++) await Promise.resolve()
  jest.advanceTimersByTime(30_000)
  await hung
  expect(shown).toEqual([
    'Context gauge: the last request sent 195,316 input tokens. Warn line 300,000, land line 380,000.',
    'Context gauge: this thread has no recorded request yet.',
    'Context gauge: no active thread.',
    'Context gauge: the reading failed: Error: read failed',
    'Context gauge: the reading failed: Error: No answer from Amp within 30 seconds',
  ])
  expect(state.appends).toEqual([])
})
