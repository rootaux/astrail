Astrail
===

Astrail is a Java-focused fork of [Joern](https://github.com/joernio/joern), trimmed down to a single language and extended with pointer-analysis-backed call resolution.

## Origin

This is a fork of [joernio/joern](https://github.com/joernio/joern). The upstream project is a general-purpose platform for analyzing source code, bytecode, and binary executables via code property graphs; all credit for the core CPG machinery, query DSL, dataflow engine scaffolding, and the Java frontends belongs there.

## Requirements

- JDK 21
- sbt

## Building

```bash
sbt compile
sbt stage
```

## Running the CLI

After `sbt stage`, the launcher scripts under `bin/` start the REPL and the
auxiliary tools. They are thin wrappers that exec the sbt-staged binaries
under `joern-cli/target/universal/stage/`, so as long as `sbt stage` has been
run at least once they Just Work from the repo root:

```bash
./bin/astrail              # interactive REPL
./bin/astrail-parse ...    # parse-only frontend driver
./bin/astrail-flow ...     # standalone reachableByFlows driver
./bin/astrail --server     # CPGQL HTTP server (add --server-quiet for SDK use)
```

Pointer analysis runs automatically as part of the `callgraph` layer, so
`reachableByFlows` queries against a freshly created CPG will use the precise
resolver without any extra setup.

```scala
importCode("path/to/project")         // javasrc2cpg or jimple2cpg picked from input
cpg.method.name("sink").reachableByFlows(cpg.method.name("source"))
```