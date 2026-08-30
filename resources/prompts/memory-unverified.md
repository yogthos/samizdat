Nothing was stored. That memory says the work is finished, and this branch has changed no files.

A claim like that is the one kind of memory the harness can check, and it is the one kind that does real damage when it is wrong: memories outlive the run that wrote them, so a later worker recalls this, concludes its part is already done, and stops. That has happened here — two such memories from failed runs told a later run its work was complete, and it shipped saying it had not made the change.

Prototyping in `eval` is not finishing. The deliverable is the edited file on disk.

If the work IS done, write it to a file first and the claim becomes true and recordable. If it is not done, there is still something worth keeping — record what you LEARNED rather than what you finished: the incantation that worked, the API that behaves differently than documented, the dead end and why. Those are facts about the project and they are useful whether or not this attempt lands.
