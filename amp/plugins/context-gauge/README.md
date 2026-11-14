# context-gauge

Amp shows no "input context used" indicator and compacts a thread automatically when its context fills. This Amp plugin watches context size so the agent does not have to remember to: after each tool result it reads the input tokens of the thread's last request and, when a line is crossed, sends the thread one notice. This is a copy of the plugin I run in my work repository; keep the two in step by copying `index.ts` and the test over whole.

## What it does

- **Reading.** The newest assistant message that recorded usage, with `inputTokens + cacheReadInputTokens + cacheCreationInputTokens`. This is the provider's count for that request, the same number as `usage.totalInputTokens` in `amp threads export`.
- **Warn line (default 300,000).** One notice asking the agent to finish its step, record the state a successor needs, and continue.
- **Land line (default 380,000).** One notice asking the agent to stop starting work, record state, and ask Peter whether to land the plane.
- **Once per context window.** Each line fires once. Both arm again after a compaction, which the gauge recognizes as a request whose reading is below 80% of the request before it: context only grows within one window. Each check looks at the last 8 requests, so a fall is seen even when several requests finished since the previous check, and the new window starts even if its first notice is deferred or fails. Changing the lines does not arm them again. A thread that first reports past the land line gets the land notice only.
- **Delivery.** The notice is appended to the thread as a steering user message, so a running agent sees it between tool calls, and it stays in the transcript. Amp also shows a toast. A notice grants no approvals. If the thread is idle when a notice is due, the gauge logs `deferred`, shows the toast and appends nothing, because an append to an idle thread starts a turn nobody asked for; the first tool result of the thread's next turn sends the notice. A notice appended just as a turn ends can still start one more turn.
- **Command.** `Context gauge: Show context usage` in the command palette shows the current reading and lines on demand.

The check runs in the background of the `tool.result` event: the handler returns at once, so tool results reach the model unchanged and undelayed. The gauge watches every thread in the Amp host that loaded it, not only majordomo sittings.

## Settings

Set the lines in Amp settings (`.amp/settings.json` for this workspace, or user settings). Changes apply at the next tool result, without a reload.

```json
{
  "amp.contextGauge.warnTokens": 300000,
  "amp.contextGauge.landTokens": 380000
}
```

Both must be positive integers with warn below land. An unusable pair falls back to the defaults as a whole.

The lines are absolute token counts because the plugin API does not expose a model's input limit. The defaults suit an 872,000-token window, where Amp appears to compact near 440,000 (two Opus 5.5 threads compacted at 435,000 to 439,000; not yet observed for Fable 5.2). On a 204,000 or 272,000 window, Amp compacts at about 90% of the limit, below the default warn line, so the gauge stays silent unless the lines are lowered.

## Install

Amp loads plugins from `<workspace>/.amp/plugins/*.ts` (one project) and `~/.config/amp/plugins/*.ts` (every project on this machine). The source here is outside both, so install it with a one-line wrapper. Install it in one place per Amp host: loaded twice, it sends every notice twice.

For every project on this machine:

```sh
mkdir -p ~/.config/amp/plugins &&
(set -C; cat > ~/.config/amp/plugins/context-gauge.ts <<'EOF'
export { default, description } from '../../../dotfiles/amp/plugins/context-gauge/index.ts'
EOF
)
```

The relative path assumes this repository is at `~/dotfiles`. Then run `plugins: reload` from the Amp command palette, or restart Amp. Expect the command `Context gauge: Show context usage` in the palette.

To uninstall, delete the wrapper and reload plugins.

## Limits

- State is in memory. After a plugin reload or Amp restart, a thread already past a line is told once more.
- One check runs per thread at a time, and a tool result that arrives during it triggers one more check afterwards. A failed check is logged and retried at the next tool result; a read that gets no answer in 30 seconds counts as failed. An append that never settles is logged as `append-slow` after 30 seconds and leaves that thread unchecked until the plugin is reloaded, because the gauge does not send a second append while the first may still be delivered.
- Each check reads the last 8 requests. If an append hangs while 8 or more requests finish, and the context is compacted and grows back to within 80% of its old size in that span, the compaction is missed and the new window gets no notices. In recorded threads a compaction cuts the context to 16-52% of its size, so this needs far more than 8 requests.
- Whether tool results of Task subagents reach the plugin under the subagent's own thread is not verified. If they do, a subagent past a line gets the same notice.
- A turn that makes no tool call is not checked until the next tool result.
- `ctx.thread.messages()` returns usage only with `full: true` (CLI 0.0.1794585642). If a later Amp version stops recording usage, the gauge goes silent without an error. The palette command then reports "no recorded request yet".

`.amp/in/context-gauge.jsonl`, three directories above `index.ts`, records each notice (`warn`, `land`) with the thread, reading and lines, plus `deferred`, `check-failed`, `append-slow` and `toast-failed`. Loaded from this repository that is `~/dotfiles/.amp/in/context-gauge.jsonl`. It holds Amp thread IDs, so keep it out of Git with a `/.amp/` line in `.git/info/exclude`. Importing the source in a script and running it writes to this log too; the offline tests run a copy to avoid that.

## Verification

Offline, with no Amp connection:

```sh
bun test ./amp/plugins/tests/context-gauge.test.ts
```

Live on 14 Nov 2026 in one Amp thread of the work repository (CLI 0.0.1794585642, Fable 5.2), loaded as a project plugin there, with the lines lowered in `.amp/settings.json` for the test:

- Warn line 100,000: one warn notice arrived mid-turn at 223,459 input tokens, equal to `totalInputTokens` of that request in `amp threads export`. Later tool results sent nothing.
- Land line then lowered to 150,000: one land notice arrived at 225,647.
- Settings removed: no further notices at the default lines.

- After the review fixes (single check per thread with a follow-up run, re-arm on a fall from the previous reading, idle deferral): one warn notice at 293,821 with the warn line at 100,000, so the thread-state read works in a running thread.

Not exercised live: the palette command, the toast as it appears in the TUI, idle deferral, and re-arming after a real compaction (covered offline only).
