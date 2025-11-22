# Migration to Kotlin/Native

This document describes the migration from Kotlin/JVM + GraalVM Native Image to Kotlin/Native.

## Why Kotlin/Native?

The migration was done to:
1. **Reduce memory usage**: Native memory management without JVM overhead
2. **Smaller binary size**: No JVM runtime or GraalVM metadata
3. **Faster startup**: Direct native execution without JVM warmup
4. **Simpler toolchain**: No need for GraalVM-specific configuration

## What Changed

### Build Configuration

**Before (JVM + GraalVM):**
- Used `kotlin-jvm` plugin
- Required GraalVM Native Image plugin
- Complex GraalVM build arguments for reflection and initialization
- Separate JVM and native build processes

**After (Kotlin/Native):**
- Uses `kotlin-multiplatform` plugin
- Native compilation built into Kotlin toolchain
- Simple compiler flags for optimization
- Single native build process

### Dependencies Replaced

| JVM Dependency | Kotlin/Native Alternative | Notes |
|----------------|---------------------------|-------|
| Arrow (Either, SuspendApp) | Kotlin Result type, runBlocking | Simplified functional programming |
| Logback | println/console logging | Native logging is simpler |
| Commons Codec (HMAC) | Native POSIX crypto | Currently simplified, needs proper crypto lib |
| Commons Text | Native string utilities | Built-in Kotlin string functions |
| KAML (YAML) | Custom simple YAML parser | Limited to basic YAML structure |
| Java File I/O | POSIX file APIs | Native fopen/fgets/fclose |
| Runtime.exec | POSIX popen/pclose | Native process execution |

### Source Code Changes

1. **File Structure**: Moved from `src/main/kotlin` to `src/nativeMain/kotlin`
2. **Result Handling**: Replaced Arrow's `Either` with sealed `Result` class
3. **File I/O**: Replaced `java.io.File` with `platform.posix` APIs
4. **Process Execution**: Replaced `Runtime.exec` with `popen`/`pclose`
5. **URL Encoding**: Implemented simple native URL encoding
6. **Logging**: Simplified from SLF4J/Logback to console output

## Known Limitations

### HMAC Authentication
The current HMAC implementation is simplified and uses a basic XOR approach for demonstration. 
**For production use**, you should:
- Integrate a proper crypto library (e.g., OpenSSL bindings)
- Or use Kotlin/Native crypto libraries when available

### YAML Parsing
The YAML parser is simplified and only handles basic structures like the project's config format.
For complex YAML:
- Consider converting to JSON
- Or integrate a full-featured YAML library for Kotlin/Native

### Testing
Test infrastructure needs to be rebuilt for Kotlin/Native:
- Replace JUnit with kotlin.test
- Update test runners for native compilation
- Add platform-specific testing if needed

## Performance Improvements

Expected improvements with Kotlin/Native:

1. **Memory**: ~5-10 MB (vs ~30-50 MB with JVM)
2. **Binary Size**: ~5-10 MB (vs ~50+ MB with GraalVM)
3. **Startup Time**: < 100ms (vs ~200-500ms with GraalVM)
4. **Memory Efficiency**: No GC pauses, direct memory management

## Building

### Local Development
```bash
# Compile native binary
./gradlew nativeCompile

# Run the binary
./build/bin/native/releaseExecutable/github-webhook-listener.kexe config.yaml
```

### Docker Build
```bash
# Build with Docker
docker build -f ./src/nativeMain/docker/Dockerfile.native -t github-webhook-listener .

# Run container
docker run -p 8080:8080 -v ./config.yaml:/opt/app/config/config.yaml github-webhook-listener
```

## Future Enhancements

1. **Add proper cryptography**: Integrate OpenSSL or KCrypto for HMAC
2. **Full YAML support**: Add complete YAML parser for native
3. **Platform targets**: Add support for macOS and Windows native builds
4. **Memory optimization**: Fine-tune allocator and GC settings
5. **Monitoring**: Add native performance monitoring and metrics

## Rollback Plan

If issues arise, the JVM version is preserved in git history:
- Build files: `build.gradle.kts.jvm-backup`, `settings.gradle.kts.jvm-backup`
- Source: `src/main/` and `src/test/` directories
- Docker: `src/main/docker/Dockerfile.jvm` and `Dockerfile.native`

To rollback:
```bash
git checkout HEAD~1 -- build.gradle.kts settings.gradle.kts
```
