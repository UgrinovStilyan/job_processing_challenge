# Job Processing Challenge

A simple HTTP job processing service implemented in Erlang/OTP 28 with Cowboy and Rebar3.  
The service accepts a JSON definition of tasks with dependencies, sorts them into an executable order, and can return the result as JSON or a Bash script.

---

## 📦 Prerequisites

- [Erlang/OTP 28](https://www.erlang.org/downloads)
- [Rebar3](https://rebar3.org) (version 3.25.1 or higher)

---

## 🚀 Setup & Run

### Build:

```bash
rebar3 compile
```

### Run:
```bash
rebar3 shell
```

### Test:

```bash
rebar3 eunit
rebar3 ct
```

---

## Example Requests & Expected outputs:

### Request for JSON output:

```bash
curl -X POST http://localhost:4000/jobs \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      {"name": "task-1", "command": "touch /tmp/file1"},
      {"name": "task-2", "command": "cat /tmp/file1", "requires": ["task-3"]},
      {"name": "task-3", "command": "echo \"Hello World!\" > /tmp/file1", "requires": ["task-1"]},
      {"name": "task-4", "command": "rm /tmp/file1", "requires": ["task-2","task-3"]}
    ]
  }' | jq .
```
### Output:

```json
{
  "tasks": [
    {"name":"task-1","command":"touch /tmp/file1"},
    {"name":"task-3","command":"echo \"Hello World!\" > /tmp/file1","requires":["task-1"]},
    {"name":"task-2","command":"cat /tmp/file1","requires":["task-3"]},
    {"name":"task-4","command":"rm /tmp/file1","requires":["task-2","task-3"]}
  ]
}
```

### Request for BASH output:

```bash
curl -X POST "http://localhost:4000/jobs?format=bash" \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      {"name": "task-1", "command": "touch /tmp/file1"},
      {"name": "task-2", "command": "cat /tmp/file1", "requires": ["task-3"]},
      {"name": "task-3", "command": "echo \"Hello World!\" > /tmp/file1", "requires": ["task-1"]},
      {"name": "task-4", "command": "rm /tmp/file1", "requires": ["task-2","task-3"]}
    ]
  }'
```
### Output:

```bash
#!/usr/bin/env bash
touch /tmp/file1
echo "Hello World!" > /tmp/file1
cat /tmp/file1
rm /tmp/file1
```
### Request failing for circular requirements:

```bash
curl -i -X POST http://localhost:4000/jobs \
  -H "Content-Type: application/json" \
  -d '{
    "tasks": [
      {"name": "a", "command": "echo A", "requires": ["b"]},
      {"name": "b", "command": "echo B", "requires": ["a"]}
    ]
  }'

```
### Output:

```
HTTP/1.1 400 Bad Request
content-type: application/json

{"Path":["a","b","a"],"error":"Cycle Detected!"}%      

```