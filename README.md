# echc
Simple Erlang Cache Server

## Build
    $ rebar3 compile

## How to use

    CacheName = my_cache,
    GetterFun = fun(Key) -> 3 * Key end,
    
    %% Start cache
    {ok, _} = echc:start(CacheName, GetterFun),
    
    %% Use cache
    R1 = echc:get(CacheName, 12), %% will call GetterFun and return 36
    R1 = echc:get(CacheName, 12), %% will return 36 without calculating it again
    
    %% You can do it parallel
    %% In that case it will call the GetterFun(7) only once, and all the other 
    %% processes will just wait for the result of the first GetterFun(7) call
    %% Once the result has arrived, all the processes receives it
    Self = self(),
    [spawn(fun() -> Self ! echc:get(CacheName, 7) end) || _ <- [1,2,3,4]],
    21 = receive V -> V end,
    21 = receive V -> V end,
    21 = receive V -> V end,
    21 = receive V -> V end,
    
    ...