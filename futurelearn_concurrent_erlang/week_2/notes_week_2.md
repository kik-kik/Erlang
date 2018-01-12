# Week 2 Concurrency - making code robust

## What could go wrong?
- Different concurrent processes share nothing, and only communicate via message passing.
- This removes worries about shared state and thread safety. However, there are a number of other things that could go wrong:
    - software error
    - data mismatch
    - network partition
    - hardware failure

## Fault tolerance
- Erlang philosophy: _'Let it fail'_. If anything goes wrong, let it fail, on the assumption that other parts of the system will deal with that.
- Let it fail is the key design principle in erlang.
- We simply cannot stop parts of a system failing, but we can handle those failures in a specific way.
- Erlang also has an exception handling mechanism.
- Erlang was designed for writing fault tolerant systems.
- You cannot build a fault tolerant system using only one computer (computer can crash due to hardware failure rather due to a programming error).


## Proces linking
- In erlang processes can live indefinitely (for example a loop process) or only as long as they are needed. For example, only until a first message matching a pattern is received.
- In another words, when a process comes to the end of its code it is terminated.
- A process can also terminate abnormally (fail).
- If a process dies unexpectedly the integrity of a system is broken.
- This can result in messages being sent to a non-existent processes or processes waiting on a message from a process that will never send them (this could result in a deadlock situation).
- We need some way of dealing with this.
- A system we have in erlang to deal with these situations is called _process linking_.
- we can use `link(Pid)` to link processes together, this means that if one of these processes fails, linked process also fails.
- Often we want to link a process that we have just spawned:
```erlang
Pid = spawn(?MODULE, loop, []),
link(Pid),
% more code
```
- However, what happens if Pid process dies before we link it? In the example above we have a race condition.
- A better thing to do s to link at exact time when we spawn a process, and erlang allows us to do that using `spawn_link`:
```erlang
Pid = spawn_link(?MODULE, loop, []),
% other sutff
```
- Linking mechanism allows us to clean up a system that has partially failed by taking the whole thing down, and starting it up again.
- If a process dies it sends an error to any processes that are linked to it.
- A firewall process does not die if it receives an error signal.
- To turn a process into a system process we can use `process_flag(trap_exit, true)`
- process_flag turns any error signals received into error messages which can be handled like messages.
- Now the system process will not be terminated, instead it will have a receive statement which can capture which and why a process has died:
```erlang
receive
    {'EXIT', Pid, Why}
```
- We use these system processes to build fault tolerant systems, we use them to handle the errors.
- In OTP we use trees instead of graphs to demonstrate systems.
- Children of nodes are handled(supervised) by their parent.
- We let processes crash and then let other processes detect those crashes and handle them.
- The idea is that process will crash regardless of what we do, so let's observe those crashes and let some other process handle the crash.

## Links, signals and messages
- When a process terminates abnormally it sends a signal to all the processes linked to it, with reason _killed_.
- Signal is sent instead of a message because the linked process might not have a way of dealing with the message.
- Even if it would mean that each process would have to contain extra code to deal with those situations.
- Default behaviour on rec2eiving a signal is to terminate (abnormally) yourself.
- In order to deal with a failing process we need to know that a process has failed without dying ourselves.
- This is where _process_flag_ comes in, it results in exit signals to that process being converted into messages.
- Trapping exists can be controlled per-process and dynamically, it can be turned on and off:
```erlang
% turning it on:
process_flag(trap_exit, true)
% and off:
process_flag(trap_exit, false)
```
- Trapping signals gives us a way to handle exit signals programmatically, as messages, so that we can implement a recover strategy in our code.
- exit(Pid, kill) is guaranteed to override exit trapping, so we can be sure that it will kill a process.
- Links are bidirectional.

### The observer tool
- Erlang come with a GUI-based tool called observer.
- It allows you to see information about the running appliction, and all processes running on your system at any time, including their particular Pid.
- To run observer type `observer:start()` in the erlang shell.

### Sending exit signals
- Exit can happen for a number of different reasons, e.g. division by 0
- Can be triggered from inside a process itself by calling `exit(Reason)`
- Can be called from an outside process by calling `exit(Pid, Reason)`
- Normal termination has the reason normal, any other reason is abnormal.
- There are 3 different exit types:
    - __Normal__ (initiated by: `exit(Pid, normal)`) - if linked process receives this and is not trapping errors then it does nothing. If trapping errors then it gets a message: `{'EXIT', Pid, normal}`.
    - __Abnormal__ (initiated by: `exit(Pid, Reason)`) - if received by a linked process and not trapping errors then it gets terminated abnormally. If trapping errors then it gets a message: `'Exit', Pid, Reason`.
    - __Kill__ (initiated by: `exit(Pid, kill)`) - if received by a process and not trapping it terminated abnormally. If trapping errors it also get's terminated abnormally in this case.
- If we want to bring down any processes that are trapping errors, then we have to send an explicit kill signal to each process.
- Kill signals do not propagate.


## Exception handling mechanisms
- 

## Hot code loading
- attributes to erlang's very high availability (typically erlang production systems are available 99.9999% of the time).

