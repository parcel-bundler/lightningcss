# Contributing

Welcome, we really appreciate if you're considering to contribute, the joint effort of our contributors make projects like this possible!

The goal of this document is to provide guidance on how you can get involved.

## Getting started with bug fixing

In order to make it easier to get familiar with the codebase we labeled simpler issues using [Good First Issue](https://github.com/parcel-bundler/lightningcss/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22) and [Help Wanted](https://github.com/parcel-bundler/lightningcss/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22+label%3A%22help+wanted%22).

Before starting make sure you have the following requirements installed: [git](https://git-scm.com), [Node](https://nodejs.org), [Yarn](https://yarnpkg.com) and [Rust](https://www.rust-lang.org/tools/install).

The process starts by [forking](https://docs.github.com/en/github/getting-started-with-github/fork-a-repo) the project and setup a new branch to work in. It's important that the changes are made in separated branches in order to ensure a pull request only includes the commits related to a bug or feature.

Clone the forked repository locally and install the dependencies:

```sh
git clone https://github.com/USERNAME/lightningcss.git
cd lightningcss
yarn install
```

## Testing

In order to test, you first need to build the core package:

```sh
yarn build
```

Then you can run the tests:

```sh
yarn test # js tests
cargo test # rust tests
```

## Building

There are different build targets available, with "release" being a production build:

```sh
yarn build
yarn build-release

yarn wasm:build
yarn wasm:build-release
```

Note: If you plan to build the WASM target, ensure that you have the required toolchain and binaries installed.

```sh
rustup target add wasm32-unknown-unknown
cargo install wasm-opt
```

## Website

The website is built using [Parcel](https://parceljs.org). You can start the development server by running:

```sh
yarn website:start
```
