# csv2json
pipe csv at stdin and outputs json.
sample usage:

$ cat /tmp/tmp.csv
one,"two",'three''s'
test,test

$ csv2json-exe < /tmp/tmp.csv
[
    {
        "c1": "one",
        "c3": "three's",
        "c2": "two"
    },
    {
        "c1": "test",
        "c2": "test"
    }
]
