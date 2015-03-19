Memory resident or disk based stores.

1) we start with a memory resident ets table
   use a dirty-clean strategy

efast_db:create(File) -> ok.
efast_db:open(File) -> ok | error
efast_db:read(Key) -> [Val] or [] for no value
efast_db:write(Key,Val) -> ok

Set semantics

On start
1) recover
   read db.1 - origonal
        db.2 - patches
        db.3 - patches

2) their is no locking
   reads and writes are atomic
   if you want locking do it through a lock manager

Pid = db_tranbs:new(PidBack) -> open a new transaction

read(Pid, key)
write(Pid, Key, Val)
