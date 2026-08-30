That form did not finish inside {{ms}}ms, so the project REPL was restarted to
stop it.

**Everything you had defined in this session is gone with it.** Re-`require` the
namespaces you were working in before you carry on, and expect to redefine
anything you had built up at the REPL.

A form that runs this long is usually a loop that does not terminate or a
sequence that is never realised. Narrow it — run the smallest piece that should
finish instantly — rather than re-issuing the same form with a longer timeout.
