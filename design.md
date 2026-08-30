a minimal kernel that handles mechanical things like talking to providers, accessing tools, security layer
everything else is expressed using mycelium and user modifyable
cells act as plugins, the workflow manifests represent state machine loops the agent engages in
the loops can nest you can have a high level feature loop, for example, which contains loops for implementer, reviewer, etc.
the tool comes with genereic templates for workflow manifests, those get copied to each project, then the project evolves on its own pace
there is a supervisor role whose sole job is to observe the system working, and modify the workflow to adapt to the problem


supervisor should run in a background threat, and have its own workflow

UI is a separate layer that attaches on top of the system, look at https://github.com/pingdotgg/t3code
