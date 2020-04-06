# npm.el
[![MELPA](https://melpa.org/packages/npm-badge.svg)](https://melpa.org/#/npm)

This is an npm client for emacs.

## Install
This package is available on MELPA

### Using `use-package`

``` emacs-lisp
(use-package npm
    :ensure t)
```

## Usage
`M-x npm`

![Startup screen for npm](assets/npm-startup.png "NPM startup")
![Install screen for npm](assets/npm-install.png "NPM install")

As of right now this package only supports the `npm run, test, install, and update` commands but I hope to add more functionality soon. If you have a command you would really like to see added here please open an issue!
