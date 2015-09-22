package me.piebridge.prevent.xposed;

import android.appwidget.AppWidgetManager;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageParser;
import android.net.Uri;
import android.os.Build;

import java.util.Set;

import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XposedHelpers;
import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.ActivityManagerServiceHook;
import me.piebridge.prevent.framework.IntentFilterHook;
import me.piebridge.prevent.framework.IntentFilterMatchResult;
import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.framework.SystemHook;
import me.piebridge.prevent.framework.util.ActivityRecordUtils;
import me.piebridge.prevent.framework.util.BroadcastFilterUtils;
import me.piebridge.prevent.framework.util.LogcatUtils;
import me.piebridge.prevent.framework.util.ProcessRecordUtils;
import me.piebridge.prevent.framework.util.SafeActionUtils;

/**
 * Created by thom on 15/9/19.
 */
public class SystemServiceHook extends XC_MethodHook {

    private static boolean systemHooked;

    private static final ThreadLocal<String> RECEIVER_SENDER = new ThreadLocal<String>();

    private static final ThreadLocal<String> SERVICE_SENDER = new ThreadLocal<String>();

    @Override
    protected void afterHookedMethod(MethodHookParam param) throws Throwable {
        if (!systemHooked) {
            PreventLog.d("start prevent hook (system)");
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            SystemHook.setClassLoader(classLoader);
            hookActivityManagerService(classLoader);
            hookActivity(classLoader);
            XposedHelpers.findAndHookMethod("android.content.IntentFilter", classLoader, "match", String.class, String.class, String.class, Uri.class, Set.class, String.class, new IntentFilterMatchHook());
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR2) {
                XposedHelpers.findAndHookMethod("android.content.Intent", classLoader, "isExcludingStopped", new IntentExcludingStoppedHook());
            }
            if (BuildConfig.ALIPAY_DONATE || BuildConfig.WECHAT_DONATE) {
                exportActivityIfNeeded();
            }
            PreventLog.d("finish prevent hook (system)");
            systemHooked = true;
            LogcatUtils.logcat();
        }
    }

    public static void hookAllMethods(Class<?> hookClass, String methodName, XC_MethodHook callback) {
        int size = XposedBridge.hookAllMethods(hookClass, methodName, callback).size();
        if (size == 0) {
            PreventLog.e("cannot hook " + hookClass.getSimpleName() + "." + methodName);
        } else {
            PreventLog.d("hook " + size + " " + hookClass.getSimpleName() + "." + methodName);
        }
    }

    private static void hookActivityManagerService(ClassLoader classLoader) throws ClassNotFoundException, NoSuchMethodException {
        Class<?> activityManagerService = Class.forName("com.android.server.am.ActivityManagerService", false, classLoader);

        XposedBridge.hookMethod(ActivityManagerServiceHook.getStartProcessLocked(activityManagerService), new ProcessHook());

        hookAllMethods(activityManagerService, "broadcastIntent", new IntentContextHook(RECEIVER_SENDER));
        hookAllMethods(activityManagerService, "startService", new ContextHook(SERVICE_SENDER));
        hookAllMethods(activityManagerService, "bindService", new ContextHook(SERVICE_SENDER));

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            XposedBridge.hookMethod(ActivityManagerServiceHook.getCleanUpRemovedTaskLocked(activityManagerService), new CleanUpRemovedHook());
        }

        // for start home activity
        hookAllMethods(activityManagerService, "startActivity", new HomeActivityHook());

        // for move activity to back
        hookAllMethods(activityManagerService, "moveActivityTaskToBack", new BackActivityHook());

        // for app died
        hookAllMethods(activityManagerService, "handleAppDiedLocked", new AppDiedHook());

        // for android 5.1's dependency
        hookAllMethods(activityManagerService, "removeProcessLocked", new IgnoreDependencyHook());
    }

    private static class AppDiedHook extends XC_MethodHook {
        @Override
        protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
            Boolean restarting = (Boolean) param.args[0x1];
            Object processRecord = param.args[0];
            if (!restarting && !ProcessRecordUtils.isKilledByAm(processRecord)) {
                SystemHook.onAppDied(processRecord);
            }
        }
    }

    private static class BackActivityHook extends XC_MethodHook {
        @Override
        protected void afterHookedMethod(MethodHookParam param) throws Throwable {
            Boolean result = (Boolean) param.getResult();
            if (result) {
                Object activityRecord = ActivityRecordUtils.getActivityRecord(param.args[0x0]);
                SystemHook.onMoveActivityToBack(activityRecord);
            }
        }
    }

    private static class HomeActivityHook extends XC_MethodHook {
        @Override
        protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
            Intent intent = null;
            for (Object arg : param.args) {
                if (arg instanceof Intent) {
                    intent = (Intent) arg;
                    break;
                }
            }
            Object processRecord = XposedHelpers.callMethod(param.thisObject, "getRecordForAppLocked", param.args[0]);
            ApplicationInfo info = ProcessRecordUtils.getInfo(processRecord);
            if (intent != null && intent.hasCategory(Intent.CATEGORY_HOME)) {
                String sender = info == null ? "" : info.packageName;
                PreventLog.v("start activity, intent: " + intent + ", sender: " + sender);
                SystemHook.onStartHomeActivity(sender);
            }
        }
    }

    private static void exportActivityIfNeeded() {
        hookAllMethods(PackageParser.class, "parseActivity", new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                PackageParser.Activity result = (PackageParser.Activity) param.getResult();
                if (result == null) {
                    return;
                }
                ActivityInfo info = result.info;
                if (BuildConfig.ALIPAY_DONATE && PreventIntent.NAME_ALIPAY.equals(info.packageName) && PreventIntent.CLASS_ALIPAY.equals(info.name)) {
                    info.exported = true;
                }
                if (BuildConfig.WECHAT_DONATE && PreventIntent.NAME_WECHAT.equals(info.packageName) && PreventIntent.CLASS_WECHAT.equals(info.name)) {
                    info.exported = true;
                }
            }
        });
    }

    private static void hookActivity(ClassLoader classLoader) throws ClassNotFoundException {
        Class<?> applicationThread = Class.forName("android.app.ApplicationThreadProxy", false, classLoader);
        hookAllMethods(applicationThread, "scheduleLaunchActivity", new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                Object activityRecord = ActivityRecordUtils.getActivityRecord(param.args[0x1]);
                SystemHook.onStartActivity(activityRecord);
            }
        });

        hookAllMethods(applicationThread, "scheduleResumeActivity", new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                Object activityRecord = ActivityRecordUtils.getActivityRecord(param.args[0x0]);
                SystemHook.onResumeActivity(activityRecord);
            }
        });

        hookAllMethods(applicationThread, "scheduleDestroyActivity", new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                Object activityRecord = ActivityRecordUtils.getActivityRecord(param.args[0x0]);
                SystemHook.onDestroyActivity(activityRecord);
            }
        });
    }

    private static class ContextHook extends XC_MethodHook {

        private final ThreadLocal<String> context;

        public ContextHook(ThreadLocal<String> context) {
            this.context = context;
        }

        @Override
        protected void beforeHookedMethod(XC_MethodHook.MethodHookParam param) throws Throwable {
            Object caller = param.args[0];
            Object callerApp = XposedHelpers.callMethod(param.thisObject, "getRecordForAppLocked", caller);
            ApplicationInfo info = ProcessRecordUtils.getInfo(callerApp);
            String sender = info == null ? "" : info.packageName;
            context.set(sender);
        }

        @Override
        protected void afterHookedMethod(XC_MethodHook.MethodHookParam param) throws Throwable {
            context.remove();
        }
    }

    private static class IntentContextHook extends ContextHook {
        public IntentContextHook(ThreadLocal<String> context) {
            super(context);
        }

        @Override
        protected void beforeHookedMethod(XC_MethodHook.MethodHookParam param) throws Throwable {
            super.beforeHookedMethod(param);
            Intent intent = (Intent) param.args[0x1];
            if (intent != null) {
                String action = intent.getAction();
                if (AppWidgetManager.ACTION_APPWIDGET_ENABLED.equals(action)) {
                    SafeActionUtils.updateWidget(intent.getComponent(), true);
                } else if (AppWidgetManager.ACTION_APPWIDGET_DISABLED.equals(action)) {
                    SafeActionUtils.updateWidget(intent.getComponent(), false);
                }
            }
        }
    }

    private static class IgnoreDependencyHook extends XC_MethodHook {
        @Override
        protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
            String reason = String.valueOf(param.args[param.args.length - 1]);
            if (reason.startsWith("stop ")) {
                Object processRecord = param.args[0];
                // 0x5 = "stop "
                String packageName = reason.substring(0x5);
                String killPackageName = ProcessRecordUtils.getInfo(processRecord).packageName;
                if (GmsUtils.isGms(packageName) && !GmsUtils.isGms(killPackageName)) {
                    XposedHelpers.setBooleanField(processRecord, "removed", false);
                    param.setResult(false);
                }
            }
        }
    }

    private static class ProcessHook extends XC_MethodHook {
        @Override
        protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
            String sender = SERVICE_SENDER.get();
            if (sender == null) {
                sender = RECEIVER_SENDER.get();
            }
            if (!ActivityManagerServiceHook.hookBeforeStartProcessLocked(param.thisObject, param.args, sender)) {
                param.setResult(null);
            }
        }
    }

    private static class IntentExcludingStoppedHook extends XC_MethodHook {
        @Override
        protected void afterHookedMethod(XC_MethodHook.MethodHookParam param) throws Throwable {
            Boolean result = (Boolean) param.getResult();
            if (result != null && result && IntentFilterHook.isPrevent((Intent) param.thisObject)) {
                param.setResult(false);
            }
        }
    }

    private static class IntentFilterMatchHook extends XC_MethodHook {
        @Override
        protected void afterHookedMethod(MethodHookParam param) throws Throwable {
            if (IntentFilterHook.canHook((Integer) param.getResult())) {
                Object filter = param.thisObject;
                String action = (String) param.args[0];
                IntentFilterMatchResult result;
                if (filter instanceof PackageParser.ActivityIntentInfo) {
                    result = IntentFilterHook.hookActivityIntentInfo((PackageParser.ActivityIntentInfo) filter, RECEIVER_SENDER.get(), action);
                } else if (BroadcastFilterUtils.isBroadcastFilter(filter)) {
                    result = IntentFilterHook.hookBroadcastFilter(filter, param.args);
                } else {
                    result = IntentFilterMatchResult.NONE;
                }
                if (!result.isNone()) {
                    param.setResult(result.getResult());
                }
            }
        }

    }

    private static class CleanUpRemovedHook extends XC_MethodHook {
        @Override
        protected void afterHookedMethod(MethodHookParam param) throws Throwable {
            ActivityManagerServiceHook.hookAfterCleanUpRemovedTaskLocked(param.args);
        }
    }
}
