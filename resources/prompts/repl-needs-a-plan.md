Before you explore, say what you are exploring FOR.

Call plan({"files": ["the file you will change"], "tests": ["the test you will write"], "goal": "one line"}) first, then the REPL is yours.

Naming a file is a hypothesis about where the problem is. A run that skipped this step spent 238 turns re-reading an implementation that was correct, because nothing ever made it consider that the bug might be in its tests. If you are not sure which file is wrong, that uncertainty IS the thing to resolve — read the failing assertion and the code it calls, decide which of the two is lying, and name that one. You can call plan again the moment you learn otherwise.
