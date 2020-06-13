# gradle-run-mode

An Emacs minor mode to run Gradle Build Tool in compile-mode.

The goal of this mode is to be lightweight and simple. It allows running Gradle just like you would from the command line, but from anywhere within the project inside Emacs, without needing a shell, and with the integration with compile-mode.

### Limitation
`gradle-run-mode` only works with Gradle projects that use the [Gradle Wrapper](https://docs.gradle.org/current/userguide/gradle_wrapper.html). It assumes that `gradlew` is located at the project's root directory and runs it from there.

## Installation
Put `gradle-run-mode.el` somwewhere on Emacs load path. Then configure this mode to always be on:

```
(require 'gradle-run-mode)
(gradle-run-mode)
```

Or with use-package, supposing the load path is `~/.emacs.d/site-lisp`:
```
(use-package gradle-run-mode
  :load-path "site-lisp"
  :config (gradle-run-mode))
```

## Usage
* `C-c C-g b`
   + To run Gradle build task.
   
* `C-c C-g t`
   + To run the Gradle test task.

* `C-c C-g r`
   + To run Gradle by interactiely entering run arguments just like running from the command line.

* `C-c C-g v` or `M-x gradle-run-set-env`
   + To interactively set the environment variables for the project build. e.g. A project may use an environment variable to specify the deployment directory. Enter them the same way you would from the command line. e.g.
   ```
   ENVIRONMENT=testing APP_HOME=~/apps/awesome
   ```

`.dir-locals.el` can also be used to set `gradle-run-env-vars` with diferent envionment variables for diffrent projects.
