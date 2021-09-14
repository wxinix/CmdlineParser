# CmdlineParser
Light-weight and flexible command line parser for Delphi applications. Adapted from [VSoft.CommandLineParser](https://github.com/VSoftTechnologies/VSoft.CommandLineParser), with minor bug fixes and styling improvements.

No external dependencies, just one single file. Supports Delphi 10.3 and up only.

# Concept and Usage
Command line options are flags (switches) specified as the command line inputs to a program. This library provides `TOptionsRegistry` class for parsing command line inputs, implementing the following concept: 

- A program can have multiple registered commands. Each command owns multiple options. Command line inputs are composed of `commands` and `options`.
- There is one invisible unnamed command, which owns Global Options. 
- An option can be specified like: `/verbose:true`, where `/` is ***option token***, `:` is ***name value separator***. 
    - Option tokens can be one of  `--`, `-`, `/`, and `@`.  
    - Name value separator can be customized by setting the property value of `TOptionRegistry.NameValueSeparator`.

## Register Global Options
Global options are owned by an invisible unnamed command, which is managed by TOptionsRegistry.

The following code illustrates how to register a global option of `boolean` type, with `long name` as "verbose", `short name` as "v", and `help text` "verbose output". A callback is provided, so when the option is parsed, the parsed value can be saved somewhere or any necessary actions can be performed. Note in the example below that the registered option explicitly sets `RequiresValue` to be `False`, meaning the option itself will be treated as a flag that doesn't need an explicit value - the appearance of the option itself would represent the default value, for example, `true` for boolean option, `0` for integer option, if no explicit value is supplied. However, if an explicit value is indeed supplied, it will be taken. Regardless default or explicitly set, the option value  will be passed to the the callback.
```delphi
  LOption := TOptionsRegistry.RegisterOption<Boolean>('verbose','v','verbose output',
    procedure(const aValue: Boolean)
    begin      
      TGlobalOptions.Verbose := aValue;
    end);

  LOption.RequiresValue := False;
 ```
To supply the option defined above to command line, simply specify `/verbose` or `/v`.  If `RequiresValue` is  `True`, the command line must be provided with an explicit value like `/verbose:true`.

## Register Command
A command can also be supplied as part of the command line. For example, if the program accepts `help` as a command, then we can have a command line like this: `/verbose help`, where `/verbose` is the global option (that is owned by the unnamed command), and `help` is a separate, named command.

A command can have its own options. Multiple commands can be registered with the program, but at most one command can appear in command line, and it must appear ***after*** any global options. The following illustrates registering a command called `install`, and an option called `installpath` for this command:
```delphi
  LInstallCommand := TOptionsRegistry.RegisterCommand(
    'install',
    '',
    'install something',
    'install command help text',
    'usage commandsample install [options]');
    
  LOption := LInstallCommand.RegisterOption<string>('installpath','i','The exe path to install',
    procedure(const aValue: string)
    begin
      TInstallOptions.InstallPath := aValue;
    end);

  LOption.Required := True;
```
## Anonymous Options
If an option is registered through `TOptionsRegistry.RegisterAnonymousOption`, then it is an anonymous (un-named) option.  Anonymous options are identified by their respective positions, hence the order by which they are registered is important.

Anonymous options, once registered, are unconditionally required. They must be supplied in the same order as they are registered. They can be mixed with named options, but their sequencial order must be the same as they are originally registered.

# Details
The command line string should start with global options (named or unnamed), then a named command, followed by the named command's options (named or unnamed). Multiple named commands can be registered, but only one can show up in the command line. Note, all command line arguments are separated by a space, but if the argument itself has a space then use double quotes or single quotes.

* Default name value separator is colon, i.e., `:`, but can be overwritten by `TOptionsRegistry.NameValueSeparator` property.
* A command line option can be prefixed by one of the four option tokens, e.g., `--`, `-`, `/`, and `@`. For example: `--autosave:true`, where `--` is the option token, `autosave` is the option's name, `true` is its value.
* Command should not be prefixed by the token.
* Global anonymous options must appear before any named command in the command line.
* An invisible command owns global options. This command is the default command , and has no name.
* Once the parser encounters a named command (new command), all items after it will be treated as its options. Anonymous options will be searched only within the encountered command's registered anonymous options. Named options will be searched first within the encountered command's named options, and if nothing found, the search will continue with global options that have been registered.
* Anonymous options (global, or belonging to a registered command) must appear in the same order when registered (as global options, or with the command). They can be mixed with named options, but the appearing order of all anonymous options must be in the same order as they are registered.
* Anonymous option is always `Required`, meaning it must be provided in the command line. Don't confuse this with `RequiresValue`. A newly created option, unless explicitly specified, by default always requires a value, but itself is not required (unless it is an unnamed option which is always required).

Check out the [unit test cases](https://github.com/wxinix/CmdlineParser/blob/master/Tests/CmdlineParserTests.TestObject.pas) and [demos](https://github.com/wxinix/CmdlineParser/tree/master/Demos) for usages.