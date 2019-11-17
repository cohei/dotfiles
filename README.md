# Dotfiles

[![CircleCI](https://circleci.com/gh/cohei/dotfiles.svg?style=svg)](https://circleci.com/gh/cohei/dotfiles)

## Requirements

- cURL
- Git

## Installation

```shell
curl -L https://dotfiles.cohei.me | bash
```

## Development

``` shell
docker run -it $(docker build --file Dockerfile.development --quiet .)
```
