# YAM 

```Shell
# Check Code Style
stack exec hlint -- --report --ignore="Use camelCase" */src
# Check package dependencies
stack exec weeder -- . --build
# Calculate Code Lines
cloc . --exclude-dir=.stack-work
```

