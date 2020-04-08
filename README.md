# Aenea

Aenea is lightweight test orchestration and test runner tool for FsCheck.

Main project principles:
* Tests are first class values - they can be passed around as values or functions into other functions. This means tests are represented as normal F# values.
* Integrated CLI test runner - test project should be normal `dotnet` executable - it means you can run it with `dotnet run` and `dotnet watch run`. There shouldn't be separation between test project and runner.
* Developer-centric approach - tests will are run and created by developers (or at least by people with technical knowledge). As such we don't need any "human-readable" DSL like in some BDD testing frameworks, but rather we want to use power of F# as normal programming language.
* Composable tests - tests are just functions - they can be easily composed, put into groups, etc. Additionally things such as changing configuration for particular test should be also expressed as function composition, rather than some specialized custom functions.

## How to build application

1. Make sure you've installed .Net Core version defined in [global.json](global.json)
2. Run `dotnet tool restore` to install all developer tools required to build the project
3. Run `dotnet fake build` to build default target of [build script](build.fsx)
4. To run tests use `dotnet fake build -t Test`
5. To build documentation use `dotnet fake build -t Docs`

## How to release.

Create release.cmd or release.sh file (already git-ignored) with following content (sample from `cmd`, but `sh` file should be similar):

```
@echo off
cls

SET nuget-key=YOUR_NUGET_KEY
SET github-user=YOUR_GH_USERNAME
SET github-pw=YOUR_GH_PASSWORD_OR_ACCESS_TOKEN

dotnet fake build --target Release
```

## How to contribute

*Imposter syndrome disclaimer*: I want your help. No really, I do.

There might be a little voice inside that tells you you're not ready; that you need to do one more tutorial, or learn another framework, or write a few more blog posts before you can help me with this project.

I assure you, that's not the case.

This project has some clear Contribution Guidelines and expectations that you can [read here](CONTRIBUTING.md).

The contribution guidelines outline the process that you'll need to follow to get a patch merged. By making expectations and process explicit, I hope it will make it easier for you to contribute.

And you don't just have to write code. You can help out by writing documentation, tests, or even by giving feedback about this work. (And yes, that includes giving feedback about the contribution guidelines.)

Thank you for contributing!


## Contributing and copyright

The project is hosted on [GitHub](https://github.com/Krzysztof-Cieslak/Aenea) where you can report issues, fork
the project and submit pull requests.

The library is available under [MIT license](LICENSE.md), which allows modification and redistribution for both commercial and non-commercial purposes.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
