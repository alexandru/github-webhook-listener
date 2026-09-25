# Kill timed-out command process groups

The listener runs configured commands through `/bin/sh -c`. Killing only the shell can leave its subprocesses running. Start each command in a separate process group and kill that group when execution stops before completion. Subprocesses that deliberately leave the group can survive; we accept that limit rather than require platform-specific process containment.
