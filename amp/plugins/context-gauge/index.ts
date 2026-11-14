import type { PluginAPI, ThreadMessage } from '@ampcode/plugin'
import { appendFileSync, mkdirSync } from 'node:fs'
import { resolve } from 'node:path'

export const description = "Context gauge: after each tool result, reads the input tokens of the thread's last request and sends one warn and one land notice per context window. Lines: amp.contextGauge.warnTokens and amp.contextGauge.landTokens."

export type Lines = {warnTokens: number, landTokens: number}
/** The highest line a thread has been told about: 0 none, 1 warn, 2 land. */
export type Level = 0 | 1 | 2
/** One request's input size, as the provider counted it, with the ID of the message it produced. */
export type Reading = {id: string, tokens: number}
/** Kept per thread while a line has been announced: that level and the reading it was last checked at. */
export type Mark = Reading & {notified: Level}
export const defaults: Lines = {warnTokens: 300_000, landTokens: 380_000}

const directory = resolve(import.meta.dir, '../../../.amp/in')
// Context only grows within one window, so a reading under this share of the one before it is a compaction.
const rearm = 0.8
// Requests read per check. More than one can finish while a check is in flight, and each is evidence.
// Known limit, accepted by Peter on 15 Nov 2026 (review round 3): if a notice append
// hangs while this many requests or more finish, and the context is compacted and grows back to within
// `rearm` of its old size in that span, the fall is outside the read and the new window gets no notices.
// Recorded compactions cut the context to 16-52% of its size, so regrowth takes far more requests than this.
// If it ever matters, page back to the mark or identify the window by the compaction summary's message ID.
const window = 8
const slowMs = 30_000
const busy = new Set(['running', 'awaiting-approval'])
const count = (tokens: number) => tokens.toLocaleString('en-US')

/** The requests that recorded usage, oldest first. */
export function readingsFrom(messages: ThreadMessage[]): Reading[] {
  return messages.flatMap(message => {
    if (message.role !== 'assistant' || !message.usage) return []
    const {inputTokens, cacheReadInputTokens, cacheCreationInputTokens} = message.usage
    return [{id: message.id, tokens: inputTokens + (cacheReadInputTokens ?? 0) + (cacheCreationInputTokens ?? 0)}]
  })
}

/** Whether any request since the mark started a new context window. */
export function compactedSince(mark: Reading, readings: Reading[]): boolean {
  // Every reading after the marked one counts, not only the newest: the context can fall and grow back between checks.
  const unseen = readings.slice(readings.findIndex(reading => reading.id === mark.id) + 1)
  const sequence = [mark.tokens, ...unseen.map(reading => reading.tokens)]
  return sequence.some((tokens, index) => index > 0 && tokens < sequence[index - 1] * rearm)
}

/** Lines from Amp settings; an unusable pair falls back to the defaults as a whole. */
export function linesFrom(config: Record<string, unknown>): Lines {
  const warnTokens = config['contextGauge.warnTokens'] ?? defaults.warnTokens
  const landTokens = config['contextGauge.landTokens'] ?? defaults.landTokens
  if (!Number.isSafeInteger(warnTokens) || !Number.isSafeInteger(landTokens)) return defaults
  const lines = {warnTokens: warnTokens as number, landTokens: landTokens as number}
  return 0 < lines.warnTokens && lines.warnTokens < lines.landTokens ? lines : defaults
}

/** Which notice, if any, this reading calls for in a window already told about `notified`. */
export function step(notified: Level, tokens: number, lines: Lines): {notified: Level, notice?: 'warn' | 'land'} {
  const level: Level = tokens >= lines.landTokens ? 2 : tokens >= lines.warnTokens ? 1 : 0
  return level > notified ? {notified: level, notice: level === 2 ? 'land' : 'warn'} : {notified}
}

export function noticeText(notice: 'warn' | 'land', tokens: number, lines: Lines): string {
  const fact = `Context gauge: the last request in this thread sent ${count(tokens)} input tokens`
  const record = 'record the state a successor needs (in a majordomo sitting: a checkpoint comment on the sitting bead)'
  if (notice === 'warn') {
    return `${fact}, past the warn line of ${count(lines.warnTokens)}. The land line is ${count(lines.landTokens)}; Amp compacts the context automatically some way above it. Peter wants long sessions closed out before compaction. Finish the step in progress, ${record}, then carry on with any work in progress. This notice grants no approvals.`
  }
  return `${fact}, past the land line of ${count(lines.landTokens)}. Peter wants long sessions closed out before Amp compacts the context. Bring work in progress to a safe stopping point, start nothing new, ${record}, and ask Peter whether to land the plane now. This notice grants no approvals.`
}

export default function (amp: PluginAPI) {
  const marks = new Map<string, Mark>()
  // The highest line Peter has had a toast for, so a deferred notice is not announced again on each retry or on delivery.
  const toasted = new Map<string, Level>()
  // Threads with a check in flight; true once another tool result arrived during it.
  const checking = new Map<string, boolean>()
  const log = (event: string, data: object = {}) => {
    try {
      mkdirSync(directory, {recursive: true})
      appendFileSync(resolve(directory, 'context-gauge.jsonl'), JSON.stringify({at: new Date().toISOString(), event, ...data}) + '\n')
    } catch (error) { amp.logger.log(`context-gauge log unavailable: ${String(error)}`) }
  }
  // Reads can be given up on and repeated; an append cannot, because it may still be delivered.
  const within = <T>(work: Promise<T>): Promise<T> => {
    let timer: ReturnType<typeof setTimeout> | undefined
    return Promise.race([work, new Promise<never>((_, reject) => {
      timer = setTimeout(() => reject(new Error(`No answer from Amp within ${slowMs / 1000} seconds`)), slowMs)
    })]).finally(() => clearTimeout(timer))
  }
  const toast = (threadID: string, level: Level, text: string) => {
    if ((toasted.get(threadID) ?? 0) >= level) return
    toasted.set(threadID, level)
    amp.ui.notify(`Context gauge: thread ${threadID} ${text}`).catch(error => log('toast-failed', {thread: threadID, error: String(error)}))
  }
  const read = async (threadID: string) => {
    const [messages, config] = await Promise.all([
      // Only the full transcript carries usage; the default view omits it (CLI 0.0.1794585642).
      amp.threads.get(threadID).messages({full: true, limit: window, roles: ['assistant']}),
      amp.configuration.get(),
    ])
    return {readings: readingsFrom(messages), lines: linesFrom(config)}
  }
  const run = async (threadID: string) => {
    const {readings, lines} = await within(read(threadID))
    const newest = readings.at(-1)
    if (!newest) return
    const {tokens} = newest
    let mark = marks.get(threadID)
    // A compaction starts a new window. The old mark goes now, so the reset holds even when this window's
    // first notice is deferred or fails.
    if (mark && compactedSince(mark, readings)) { marks.delete(threadID); toasted.delete(threadID); mark = undefined }
    const next = step(mark?.notified ?? 0, tokens, lines)
    if (next.notice) {
      const thread = amp.threads.get(threadID)
      // An append to an idle thread would start a turn nobody asked for. Nothing is remembered,
      // so the first tool result of the thread's next turn sends the notice.
      const state = await within(thread.state.get())
      if (!busy.has(state)) {
        log('deferred', {thread: threadID, notice: next.notice, tokens, state})
        toast(threadID, next.notified, `passed the ${next.notice} line at ${count(tokens)} input tokens while idle; its agent is told at its next tool result.`)
        return
      }
      // An append that never settles keeps this thread unchecked; say so instead of risking a duplicate.
      const slow = setTimeout(() => log('append-slow', {thread: threadID, notice: next.notice, seconds: slowMs / 1000}), slowMs)
      try { await thread.appendUserMessage({type: 'user-message', content: noticeText(next.notice, tokens, lines)}, {steer: true}) }
      finally { clearTimeout(slow) }
      log(next.notice, {thread: threadID, tokens, ...lines})
      toast(threadID, next.notified, `passed the ${next.notice} line at ${count(tokens)} input tokens.`)
    }
    // Remembered only after the append succeeded, so a failed append is retried on the next tool result.
    if (next.notified) marks.set(threadID, {...newest, notified: next.notified})
    else { marks.delete(threadID); toasted.delete(threadID) }
  }
  const check = async (threadID: string) => {
    // One check per thread at a time: an append whose receipt is pending must not be sent twice.
    // A tool result that arrives meanwhile may belong to a newer request, so it asks for one more run.
    if (checking.has(threadID)) { checking.set(threadID, true); return }
    do {
      checking.set(threadID, false)
      try { await run(threadID) }
      catch (error) { log('check-failed', {thread: threadID, error: String(error)}) }
    } while (checking.get(threadID))
    checking.delete(threadID)
  }

  // Returns at once with no result, so the tool result reaches the model unchanged and undelayed.
  amp.on('tool.result', event => { void check(event.thread.id) })

  amp.registerCommand('context-gauge-show', {
    title: 'Show context usage', category: 'Context gauge',
    description: "Show the input tokens of this thread's last request and the warn and land lines",
  }, async ctx => {
    if (!ctx.thread) { await ctx.ui.notify('Context gauge: no active thread.'); return }
    const reading = await within(read(ctx.thread.id)).catch(error => String(error))
    if (typeof reading === 'string') { await ctx.ui.notify(`Context gauge: the reading failed: ${reading}`); return }
    const tokens = reading.readings.at(-1)?.tokens
    const {lines} = reading
    await ctx.ui.notify(tokens === undefined
      ? 'Context gauge: this thread has no recorded request yet.'
      : `Context gauge: the last request sent ${count(tokens)} input tokens. Warn line ${count(lines.warnTokens)}, land line ${count(lines.landTokens)}.`)
  })
}
