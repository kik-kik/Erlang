# Functional Erlang
- Erlang at it's core is functional.
- Works if immutable data.
- But it is not as pure as Haskell.
- Disallows some side-effects (e.g. changing variable values), but does allow others (such communication).
- Erlang is designed to build solutions to particular problems.

## Message passing concurrency
- Different concurrent process share nothing.
- They only communicate by passing messages between each other.
- This leads to thraed safety, no worries about shared state.
- Concurrency across different hosts.
- Messages send and received are decoupled in erlang (process does not have to wait for the receiver to open the message or respond to continue).
- This helps to avoid common source of deadlocks.
- This approach is easier to implement in network distributed systems than when using a synchronous approach.
- Can simulate synchrony through send/receive protocol.
- Messages are sent to a specific process id (Pid), rather than along a channel between processes.
- In erlang we can send a message to any process as long as we know its Pid.
- Names can be associated with Pid's.
- However, this results in less evident communication structure.

## The actor model
- Carl Hewitt's model for foundations of computation
- everything is an actor.
- actors communicate using messages.
- actors can create other actors.
- actors can change behaviour in response to messages.

## Concurrency vs parallelism
- Concurrency means the possibily of running independently, perhaps at the same time.
- On a single processing element, concurrent processes timeshare controller by a scheduler.
- On a multicore processor it is possible to have those processes running in parallel.

## Erlang concurrency
- In erlang we use processes instead of threads.
- Processes share nothing with each other.
- Processes in erlang exist in the virtual machine.
- They are more lightweight than threads, more numerous, and more control (seperate stack, heap and garbage collection per process).
- spawn - a function to create a process.
- ! (exclamation mark, referred to as _pling_ or _bang_) - operator used to send a message to a process.
- self() - a function that returns a process id of the process calling it.
- receive construct - provides a way of handling a message.

### process
- A process is a separate computation that runs in its own space, and time-shares with other processes.
- Process runs an erlang function which terminates when/if that function terminates.
- we use spawn function to create a process.

### spawn
```erlang
spawn(Module, Function_name, [Args])

% example:
spawn(foo, bar, []).
```
- spawn returns a process id of the process created which can be captured to refer to that process in the future:
`Proc = spawn(fun, mod, []).`
- This enables us to send messages to that process.
- If messages are received and not processed they get stored in the mailbox.

### mailbox
- We can clear the mailbox using `flush()` function.


### receive
```erlang
receive
    Msg -> handle Msg
end
```
- Takes Msg out of the mailbox and handles it.
- The general receive construct allows us to pattern match on messages and choose between alternatives (this case statement) e.g:
```erlang
receive
    stop -> io:format("stopping~n");
    Msg -> io:format("received ~s~n", [Msg]),
            call_itself()
end.
```

