# todo-backend


[![CircleCI](https://circleci.com/gh/yourgithubhandle/todo-backend/tree/master.svg?style=svg)](https://circleci.com/gh/yourgithubhandle/todo-backend/tree/master)


**Contains the following libraries and executables:**

```
todo-backend@0.0.0
│
├─test/
│   name:    TestTodoBackend.exe
│   main:    TestTodoBackend
│   require: todo-backend.lib
│
├─library/
│   library name: todo-backend.lib
│   namespace:    TodoBackend
│   require:
│
└─executable/
    name:    TodoBackendApp.exe
    main:    TodoBackendApp
    require: todo-backend.lib
```

## Developing:

```
npm install -g esy
git clone <this-repo>
esy install
esy build
```

## Running Binary:

After building the project, you can run the main binary that is produced.
For that you need to provide the connection url to your postgres database.
An example of such a url could be: `postgres://hamza0867:1234@localhost:5432/todo`.

```
CONN_STRING=postgres://USER_NAME:PASSWORD@DB_HOST:DB_PORT/DB_NAME esy start
```

## Running Tests:

```
# Runs the "test" command in `package.json`.
esy test
```
