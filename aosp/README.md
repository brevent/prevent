# How To Patch (English, Chinese see below)

## Requirements

- Python, latest 2.X or 3.X
- JDK 7+, smali/baksmali requires Java 7+
- baksmali/smali, 2.2+ for Android 7.0+
- oat2dex, for Android 5.0 - 5.1

## Get services.jar/services.odex/boot.oat from devices

```
shell> adb pull ...
```

## Convert services to smali

### Android 4.4 or non-odex version (services.jar > 1M)
shell> java -jar baksmali-2.2b4.jar d services.jar -o services

### Android 5.0 - Android 5.1
```
shell> java -jar oat2dex-0.8.6.jar boot boot.oat
shell> java -jar oat2dex-0.8.6.jar services.odex dex
shell> java -jar baksmali-2.2b4.jar d services.dex -o services
```

### Android 6.0 - Android 7.1

```
shell> java -jar baksmali-2.2b4.jar x -d <dir/to/boot.oat> <path/to/services.odex> -o services
```

## Convert apk to smali

```
shell> java -jar baksmali-2.2b4.jar d <path/to/Brevent APK> -o apk
```

## Patch it

Get `patch.py` from apk, you can rename apk to zip, then find it in `assets/patch.py`.
```
shell> python <path/to/patch.py> -a apk -s services
```

## Convert patched services to services.jar
```
shell> java -jar smali-2.2b4.jar a -o classes.dex services
shell> jar -cvf services.jar classes.dex
```

# 怎样打补丁

## 要求

- Python, 最新的 2.X 或者 3.X
- JDK 8+, smali/baksmali 需要 Java 7，而 oat2dex 需要Java 8
- baksmali/smali, 2.2以上才支持 Android 7.0
- oat2dex, 因为 baksmali/smali 不支持 Android 5.0 - 5.1

## 从设备中获取 services.jar, services.odex, boot.oat

```
shell> adb pull ...
```

## 把 services 转成 smali

### Android 4.4 或者非 odex 优化版本 (services.jar 在 1M 以上)

```
shell> java -jar baksmali-2.2b4.jar d services.jar -o services
```

### Android 5.0 - Android 5.1

```
shell> java -jar oat2dex-0.8.6.jar boot boot.oat
shell> java -jar oat2dex-0.8.6.jar services.odex dex
shell> java -jar baksmali-2.2b4.jar d services.dex -o services
```

### Android 6.0 - Android 7.1

```
shell> java -jar baksmali-2.2b4.jar x -d <boot.oat所在目录(!不是文件!)> <services.odex文件路径> -o services
```

## 把 apk 转成 smali

因为担心并不随时可以访问网络，所以把部分代码放到了安装包中。

```
shell> java -jar baksmali-2.2b4.jar d <黑域APK路径> -o apk
```

## 打补丁

从安装包中获取 patch.py，可以直接把 apk 后缀改为 zip，就能看到 assets/patch.py 了。

```
shell> python <patch.py路径> -a apk -s services
```

正常的话，Android 4.4 一共有 14 处补丁，5.0 以上共有 15 处补丁。

## 输出打过补丁的 services

```
shell> java -jar smali-2.2b4.jar a -o classes.dex services
shell> jar -cvf services.jar classes.dex
```
