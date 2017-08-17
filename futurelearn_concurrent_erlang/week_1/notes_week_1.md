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
- Erlang only gives a promise on the order in which messages will be delivered if they were sent from the same process, otherwise anything is possible.
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
- One of the problems we may see if that of race conditions.
- This is that we are unsure in what order things happen which depending on the order of execution we may get different results.

## process
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
- When we send a message we do not actually send it to a process, but to its mailbox instead.
- Each process has a mailbox, a message gets put into a mailbox of the target process and the receive statement looks through the mailbox for any messages that pattern match.
- If a message matches a pattern then it is taken out of the mailbox.
- This approach (a queueing system in the form of a mailbox) is necessary because of the fact message passing in erlang is asynchronous.
The mailbox works in the following way:
    - try the clauses in turn with the first message in the mailbox; if one matches, match the message with the pattern in the first matching clause, execute the corresponding code, and remove the message from the mailbox;
    - if none of the clauses matches the first message, repeat with the second message, and continue until all messages are checked;
    - if none of the messages is matched, wait for a message that will be matched.
- We can flush the mailbox by doing something as follows:
```erlang
clear() ->
    receive
        _Msg -> clear()
    after 0 ->
        ok
    end.
```
- This will run as long as there is any number of messages in the mailbox, if not it'll just straight to after and execute the code found the statement. In this case, it'll return ok and exit the function.

### receive
```erlang
receive
    Msg -> handle Msg
end
```
- Takes Msg out of the mailbox and handles it.
- receive is selective, it allows us to pattern match on messages and choose between alternatives (this case statement) e.g:
```erlang
receive
    stop -> io:format("stopping~n");
    Msg -> io:format("received ~s~n", [Msg]),
            call_itself()
end.
```
- if no messages match, we will have to wait until a message that matches arrives before proceeding.


### after
- perhaps we want to set a timeout limit when waiting for a response to avoid our process waiting infinitely.
- we can use after clause for that:
```erlang
receive
    Pattern1 ->
        Actions1;
    PaternN ->
        ActionsN
    after Time ->
        TimeoutActions
    end
```
- this tells our process to wait a certain amount of time and if no response was received we want other actions to be carried out.
- Potential problem with this is that a message may arrive late after the specified time has elapsed and the message would be left in the mailbox till later.

### Registering processes
- Only parent knowns its child.
- to register a process we can use:
```erlang
register(atom_name, Pid)
% another example:
Server = spawn(module, function, [Args]),
register(server, Server),
server ! {self(), ping}
% OR
register(server, spawn(module, function, [Args])),
```
- In the line above, server becomes a symbolic link to the spawned process.
- Anyone can now send a message to the process linked to the atom (server in this case), they don't have to have the Pid themselves.
- Typically, spawn and register are used in one nested call as seen above.
- we can use the registered process name to then find out what process id is associated to a specific name:
```erlang
Pid = whereis(atom_name)
```
- Some differences between names and pids:
    - Sending a message to a none-existent pid it just disappears silently.
    - Sending a message to a none-existent name of a process we get an error.
- Typically, we name static, long-lived processes.
- A convention in erlang is that the named process gets its name from the module where it is defined (assuming there is only one per module).

## Abstracting patterns of concurrency

### RPC (Remote Procedure Call)
- It consists of sending a message to a remote computer and then waiting for a response.
- It takes two arguments, a Pid and a request.
- Example:
```erlang
rpc(Pid, Request) ->
    Tag = erlang:make_ref(),
    Pid ! {self(), Tag, Request},
    receive
        {Tag, Response} -> Response
    after 20 ->
        timed_out
    end.
```
- RPC split into two functions:
```erlang
rpc(Pid, Request) ->
    Tag = erlang_make_ref(),
    Pid ! {self(), Tag, Request},
    Tag.

wait_response(Tag) ->
    receive
        {Tag, Response} ->
            Response
    end.
```
- Another way to look at the above is in terms of _futures_:
```erlang
promise(Pid, Request) ->
    Tag = erlang_make_ref(),
    Pid ! {self(), Tag, Request},
    Tag.

yield(Tag) ->
    receive
        {Tag, Response} ->
            Response
    end.

% to use this we do something like this:
Tag = promise(Pid, fun() -> ... end),
... do some computations...
Val = yield(Tag).
```
