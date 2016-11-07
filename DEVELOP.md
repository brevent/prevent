# English
For IDE (Android Studio), pleaes import `build.gradle`.

For APK, please use `gradle -b release.gradle clean aR`.
You can specify the key in file `ant.properties` (please create it) like this:
```
key.store=
key.alias=
key.store.password=
key.alias.password=
```

Please note, there is no `framework` part. I won't release the source code for `framework` until original Prevent Running supports Android 7.0+.

# 中文
如果使用 IDE (Android Studio)，请直接使用`build.gradle`。

如果要生成 APK 的话，请使用命令`gradle -b release.gradle clean aR`。
你可以在文件`ant.properties`中使用以下参数来指定分发密钥：
```
key.store=
key.alias=
key.store.password=
key.alias.password=
```

注意：源码中没有`framework`，在原始`阻止运行`支持 Android 7.0 之前，不会开放 `framework` 代码。
