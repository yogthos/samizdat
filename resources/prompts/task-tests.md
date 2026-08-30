A test namespace beside every namespace you add, registered in this project's test runner, and the suite green before you close this task.

Write the test for a piece BEFORE the piece, and run it so you watch it fail for the reason you expect. A test written after the code has never been seen to fail, so it is not yet evidence of anything — it may be asserting something that was already true.

At least one test must drive a SEAM rather than a single namespace: feed one layer the exact data shape the layer below it actually produces, over enough steps to do the thing a user would. Per-namespace tests each check one half against a contract they invented, and two correct halves with disagreeing contracts is the defect they cannot see. The last project here shipped a green suite of 47 tests while every frame of the running program threw, for exactly that reason.
