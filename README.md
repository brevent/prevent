# English

Android 3.1 introduces Launch controls on stopped applications: http://developer.android.com/about/versions/android-3.1.html#launchcontrols . However, there are no such features on android 2.3, and some devices didn't implement it at all.

The module hajacks several system api to prevent the broadcast being sent to the prevented packages. Furthermore, it applies to system packages too (compares to native launch controls), but the system packages must have a launcher activity. (If you want to prevent these system packages from running, please consider disable instead).

Please note, this application prevents following actions:

- (when prevents) suicide in activity, which will restart service
- (when prevents) receive event which may change default home action
- (when prevents) move task to background
- (when prevents) start home activity
- (when prevents) process with parent to init

**WARNING**: please don't prevents system pacakges nor daily packages. Otherwise, you may miss important information.

"Prevent Running" should work from android 2.3 to 5.1. However, I mainly use 4.4.

How to use:

1. install "Prevent Running", activate it in "Xposed Installer", reboot.
2. open "Prevent Running",  then add/remove application to/from prevent list.
If you don't like "auto prevent" added packages, please prevent "Prevent Running".

Special Search:

- `-3` for third party apps
- `-s` for system apps
- `-e` for non-prevent apps
- `-r` for running apps

Project: https://github.com/liudongmiao/ForceStopGB , any donations are welcome.

# 中文

安卓3.1对强行停止的程序引入了启动控制( http://developer.android.com/about/versions/android-3.1.html#launchcontrols )。但是，在2.3没有这个功能，而有些安卓4.X的设备根本没有实现。再者，很多流氓，总是有办法不断启动。

“阻止运行”通过劫持几个系统API，不让广播发送到没有运行的阻止程序，同时(相比安卓原生)支持有图标的系统应用。(如果你想阻止没有图标的系统应用，请考虑直接禁用它。)

“阻止运行”禁止以下行为：

- (当程序被阻止时) 自杀，如搜狗地图。
- (当程序被阻止时) 接收系统按键，主要是HOME键，如支付宝。
- (当程序被阻止时) 程序自己移到后台，如微信。
- (当程序被阻止时) 程序启动HOME，如微博。
- (当程序被阻止时) 产生一些游离系统外的进程，如应用宝。

**警告**：请谨慎阻止“系统应用”，以及常用应用。要不然，你可能无法及时收到短信或其它重要消息。

这个模块支持安卓2.3到5.1，个人主要在4.4上测试。(2.3请安装本人移植的xposed框架。）

使用说明：

1. 安装“阻止运行”，在Xposed Installer中激活它，重启（必须）。
2. 重启后，打开“阻止运行”，配置“阻止列表”(这个只需要一次)。
“阻止运行”自动把新装应用添加到“阻止列表”，如果不想这样，请把阻止运行添加到阻止列表（不建议）。

特别搜索：

- `-3` 用户安装的第三方程序
- `-s` 系统预装的程序
- `-e` 没有阻止的程序
- `-r` 正在运行的程序

“阻止运行”开源，项目地址：https://github.com/liudongmiao/ForceStopGB ，欢迎各式捐赠，以身相许除外。
