# Prefetch calibration with NaCl


## How to build it

For building you need to download and install the google NaCl / pepper SDK from this [webpage](https://developer.chrome.com/native-client/sdk/download).
This code has been tested with the version pepper_49 (latest stable version to date).

- Set the `NACL_SDK_ROOT` environment variable to the SDK root (eg, `~/nacl_sdk/pepper_49`).

- Compile:

```
$ make
```


## How to run it

```
$ $NACL_SDK_ROOT/tools/sel_ldr.py glibc/Release/calibration_x86_64.nexe
```

