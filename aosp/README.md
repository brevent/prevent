# How To Patch (English, Chinese see below)

## Requirements

- `adb`, part of Android platform tools
  - [Windows][adb-win]
    - [USB Driver][adb-win-driver], for old windows
  - [Linux][adb-linux]
  - [Mac OS X][adb-mac]
- [Python][python], latest 2.X or 3.X
- [Java][javase], Java SE 8 (JDK)
- [baksmali/smali][smali], 2.2+ for Android 7.0+
- [patch.py][patch.py](optional), can get from Brevent APK
- [oat2dex][oat2dex](optional), v0.86 for Android 5.0 - 5.1

## Get services.jar/services.odex/boot.oat from devices

```
shell> adb pull /system/framework
```

## Convert services to smali

### Android 4.4 or non-odex version (services.jar > 1M)
shell> java -Xms1g -jar baksmali-2.2b4.jar d services.jar -o services

### Android 5.0 - Android 5.1
```
shell> java -Xms1g -jar oat2dex.jar boot <path/to/boot.oat>
shell> java -Xms1g -jar oat2dex.jar <path/to/services.odex> dex
shell> java -Xms1g -jar baksmali-2.2b4.jar d <path/to/services.dex> -o services
```

### Android 6.0 - Android 7.1

```
shell> java -Xms1g -jar baksmali-2.2b4.jar x -d <dir/to/boot.oat> <path/to/services.odex> -o services
```

## Convert apk to smali

```
shell> java -Xms1g -jar baksmali-2.2b4.jar d <path/to/Brevent APK> -o apk
```

## Patch it

Get `patch.py` from apk, you can rename apk to zip, then find it in `assets/patch.py`. If you can get [patch.py][patch.py], then use it direct instead of the one in the apk.

```
shell> python <path/to/patch.py> -a apk -s services
```

## Convert patched services to services.jar
```
shell> java -Xms1g -jar smali-2.2b4.jar a -o classes.dex services
shell> jar -cvf services.jar classes.dex
```

# 怎样打补丁

* 说明

```
命令行> 命令
```

表示在`电脑`的`命令行`中执行`命令`，如果没有特别声明，Linux / Mac OS X / Windows 下均可使用。

```
<XXX路径>
```

表示`XXX`在`电脑`里的路径，不是`设备`里的路径。

## 要求

- `adb`, 存在于 Android platform tools 中
  - [Windows][adb-win]
    - [USB 驱动][adb-win-driver], 某些古老的 windows 可能需要
  - [Linux][adb-linux]
  - [Mac OS X][adb-mac]
- [Python][python], 最新的 2.X 或者 3.X，用于运行补丁程序
- [Java][javase], Java SE 8 (JDK) 以上，smali/baksmali 需要 Java 7，而 oat2dex 需要Java 8
- [baksmali/smali][smali], 2.2 及以上版本，用于解包封包
- [patch.py][patch.py](可选), 因为也能从黑域 APK 中提取
- [oat2dex][oat2dex](可选), 版本 v0.86，用于 Android 5.0 与 5.1 解包，因为 baksmali 不支持 Android 5.0 - 5.1

## 从设备中获取 services.jar, services.odex, boot.oat

```
命令行> adb pull /system/framework
```

## 把 services 转成 smali

### Android 4.4 或者非 odex 优化版本 (services.jar 在 1M 以上)

```
命令行> java -Xms1g -jar baksmali-2.2b4.jar d <services.jar路径,1M以上> -o services
```

### Android 5.0 - Android 5.1

```
命令行> java -Xms1g -jar oat2dex.jar boot <boot.oat路径>
命令行> java -Xms1g -jar oat2dex.jar <services.odex路径> <dex目录路径>
命令行> java -Xms1g -jar baksmali-2.2b4.jar d <services.dex路径> -o services
```

### Android 6.0 - Android 7.1

```
命令行> java -Xms1g -jar baksmali-2.2b4.jar x -d <boot.oat所在目录(!不是文件!)> <services.odex文件路径> -o services
```

## 把 apk 转成 smali

因为担心并不随时可以访问网络，所以把部分代码放到了安装包中。

```
命令行> java -Xms1g -jar baksmali-2.2b4.jar d <黑域APK路径> -o apk
```

## 打补丁

从安装包中获取 patch.py，可以直接把 apk 后缀改为 zip，就能看到 assets/patch.py 了。如果你能直接下载到最新版的 [patch.py][patch.py]，请直接使用它即可。

```
命令行> python <patch.py路径> -a apk -s services
```

正常的话，Android 4.4 一共有 14 处补丁，5.0 以上共有 15 处补丁。

## 输出打过补丁的 services

```
命令行> java -Xms1g -jar smali-2.2b4.jar a -o classes.dex services
命令行> jar -cvf services.jar classes.dex
```

[adb-win]: http://dl.google.com/android/repository/platform-tools_r25-windows.zip
[adb-mac]: http://dl.google.com/android/repository/platform-tools_r25-macosx.zip
[adb-linux]: http://dl.google.com/android/repository/platform-tools_r25-linux.zip
[adb-win-driver]: http://dl.google.com/android/repository/usb_driver_r11-windows.zip
[javase]: http://www.oracle.com/technetwork/java/javase/downloads/index.html
[python]: https://www.python.org/downloads/
[smali]: https://bitbucket.org/JesusFreke/smali/downloads
[patch.py]: https://github.com/liudongmiao/Brevent/raw/master/assets/patch.py
[oat2dex]: https://github.com/testwhat/SmaliEx/releases/tag/0.86
