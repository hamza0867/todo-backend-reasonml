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

```
esy x TodoBackendApp.exe 
```

## Running Tests:

```
# Runs the "test" command in `package.json`.
esy test
```
