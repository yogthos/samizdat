**Your first decision, before any code: is this task ONE thing or SEVERAL?**
Count the distinct capabilities it asks for. A task naming two or more IS more
than one thing, whatever their size and whatever file they land in.

One capability: implement it yourself. Two or more: you are the architect of
this task, not its implementor — design the boundary, write the stubs, write the
composition that calls them, and `split`. Implementing several capabilities
yourself is the choice that needs justifying, not the default.
