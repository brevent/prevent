package me.piebridge.prevent.xposed;

import android.app.IApplicationThread;
import android.appwidget.AppWidgetManager;
import android.content.ComponentName;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageParser;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.os.ParcelFileDescriptor;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
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

    private static Method getRecordForAppLocked;

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
            hookIntentFilter(classLoader);
            hookIntentIfNeeded(classLoader);
            if (BuildConfig.ALIPAY_DONATE || BuildConfig.WECHAT_DONATE) {
                exportActivityIfNeeded();
            }
            PreventLog.d("finish prevent hook (system)");
            systemHooked = true;
            LogcatUtils.logcat();
        }
    }

    private void hookIntentIfNeeded(ClassLoader classLoader) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR2) {
            XposedHelpers.findAndHookMethod("android.content.Intent", classLoader,
                    "isExcludingStopped",
                    new IntentExcludingStoppedHook());
        }
    }

    private void hookIntentFilter(ClassLoader classLoader) {
        XposedHelpers.findAndHookMethod("android.content.IntentFilter", classLoader, "match",
                String.class, String.class, String.class, Uri.class, Set.class, String.class,
                new IntentFilterMatchHook());
    }

    private void hookActivityManagerService(ClassLoader classLoader) throws ClassNotFoundException, NoSuchMethodException {
        Class<?> activityManagerService = Class.forName("com.android.server.am.ActivityManagerService", false, classLoader);

        hookActivityManagerServiceStartProcessLocked(activityManagerService);

        hookActivityManagerServiceBroadcastIntent(activityManagerService, classLoader);

        hookActivityManagerServiceStartService(activityManagerService);

        hookActivityManagerServiceBindService(activityManagerService, classLoader);

        hookActivityManagerServiceCleanUpRemovedTaskLocked(activityManagerService, classLoader);

        hookActivityManagerServiceStartActivity(activityManagerService, classLoader);

        hookActivityManagerServiceMoveActivityTaskToBack(activityManagerService);

        hookActivityManagerServiceHandleAppDiedLocked(activityManagerService, classLoader);

        hookActivityManagerServiceRemoveProcessLocked(activityManagerService, classLoader);

        getRecordForAppLocked = activityManagerService.getDeclaredMethod("getRecordForAppLocked", IApplicationThread.class);
        getRecordForAppLocked.setAccessible(true);
    }

    private void hookActivityManagerServiceRemoveProcessLocked(Class<?> activityManagerService, ClassLoader classLoader) throws ClassNotFoundException {
        // mainly for android 5.1's dependency
        int sdk = Build.VERSION.SDK_INT;
        String method = "removeProcessLocked";
        Class<?> processRecord = Class.forName("com.android.server.am.ProcessRecord", false, classLoader);
        if (sdk >= Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1) {
            // sdk 15, sdk 16, sdk 17, sdk 18, sdk 19, sdk 21, sdk 22
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    processRecord, boolean.class, boolean.class, String.class,
                    new IgnoreDependencyHook());
        } else if (sdk >= Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            // sdk 14
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    processRecord, boolean.class, boolean.class,
                    new IgnoreDependencyHook());
        } else {
            // sdk 10
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    processRecord, boolean.class,
                    new IgnoreDependencyHook());
        }
    }

    private void hookActivityManagerServiceHandleAppDiedLocked(Class<?> activityManagerService, ClassLoader classLoader) throws ClassNotFoundException {
        int sdk = Build.VERSION.SDK_INT;
        String method = "handleAppDiedLocked";
        Class<?> processRecord = Class.forName("com.android.server.am.ProcessRecord", false, classLoader);
        if (sdk >= Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            // sdk 14, sdk 15, sdk 16, sdk 17, sdk 18, sdk 19, sdk 21, sdk 22
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    processRecord, boolean.class, boolean.class, new AppDiedHook());
        } else {
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    processRecord, boolean.class,
                    new AppDiedHook());
        }

    }

    private void hookActivityManagerServiceMoveActivityTaskToBack(Class<?> activityManagerService) {
        // for move activity to back
        // sdk 10, sdk 14, sdk 15, sdk 16, sdk 17, sdk 18, sdk 19, sdk 21, sdk 22
        XposedHelpers.findAndHookMethod(activityManagerService, "moveActivityTaskToBack",
                IBinder.class, boolean.class,
                new BackActivityHook());
    }

    private void hookActivityManagerServiceStartActivity(Class<?> activityManagerService, ClassLoader classLoader) throws ClassNotFoundException {
        // for start home activity
        int sdk = Build.VERSION.SDK_INT;
        String method = "startActivity";
        if (sdk >= Build.VERSION_CODES.LOLLIPOP) {
            // sdk 21, sdk 22
            Class<?> profilerInfo = Class.forName("android.app.ProfilerInfo", false, classLoader);
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, String.class, Intent.class, String.class, IBinder.class, String.class, int.class, int.class, profilerInfo, Bundle.class,
                    new HomeActivityHook());
        } else if (sdk >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
            // sdk 18, sdk 19
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, String.class, Intent.class, String.class, IBinder.class, String.class, int.class, int.class, String.class, ParcelFileDescriptor.class, Bundle.class,
                    new HomeActivityHook());
        } else if (sdk >= Build.VERSION_CODES.JELLY_BEAN) {
            // sdk 16, sdk 17
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, Intent.class, String.class, IBinder.class, String.class, int.class, int.class, String.class, ParcelFileDescriptor.class, Bundle.class,
                    new HomeActivityHook());
        } else if (sdk >= Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            // sdk 14, sdk 15
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, Intent.class, String.class, Uri[].class, int.class, IBinder.class, String.class, int.class, boolean.class, boolean.class, String.class, ParcelFileDescriptor.class, boolean.class,
                    new HomeActivityHook());
        } else {
            // sdk 10
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, Intent.class, String.class, Uri[].class, int.class, IBinder.class, String.class, int.class, boolean.class, boolean.class,
                    new HomeActivityHook());
        }
    }

    private void hookActivityManagerServiceCleanUpRemovedTaskLocked(Class<?> activityManagerService, ClassLoader classLoader) throws ClassNotFoundException {
        int sdk = Build.VERSION.SDK_INT;
        String method = "cleanUpRemovedTaskLocked";
        if (sdk >= Build.VERSION_CODES.LOLLIPOP_MR1) {
            // sdk 22
            Class<?> taskRecord = Class.forName("com.android.server.am.TaskRecord", false, classLoader);
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    taskRecord, boolean.class, new CleanUpRemovedHook());
        } else if (sdk >= Build.VERSION_CODES.JELLY_BEAN) {
            // sdk 16, sdk 17, sdk 18, sdk 19, sdk 21
            Class<?> taskRecord = Class.forName("com.android.server.am.TaskRecord", false, classLoader);
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    taskRecord, int.class,
                    new CleanUpRemovedHook());
        } else if (sdk >= Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            // sdk 14, sdk 15
            Class<?> activityRecord = Class.forName("com.android.server.am.ActivityRecord", false, classLoader);
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    activityRecord, boolean.class,
                    new CleanUpRemovedHook());
        }
    }

    private void hookActivityManagerServiceBindService(Class<?> activityManagerService, ClassLoader classLoader) throws ClassNotFoundException {
        int sdk = Build.VERSION.SDK_INT;
        String method = "bindService";
        Class<?> iServiceConnection = Class.forName("android.app.IServiceConnection", false, classLoader);
        if (sdk >= Build.VERSION_CODES.JELLY_BEAN) {
            // sdk 16, sdk 17, sdk 18, sdk 19, sdk 21, sdk 22
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, IBinder.class, Intent.class, String.class, iServiceConnection, int.class, int.class,
                    new ContextHook(SERVICE_SENDER));
        } else {
            // sdk 10, sdk 14, sdk 15
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, IBinder.class, Intent.class, String.class, iServiceConnection, int.class,
                    new ContextHook(SERVICE_SENDER));
        }
    }

    private void hookActivityManagerServiceStartService(Class<?> activityManagerService) {
        int sdk = Build.VERSION.SDK_INT;
        String method = "startService";
        if (sdk >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
            // sdk 18, sdk 19, sdk 21, sdk 22
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, Intent.class, String.class, int.class,
                    new ContextHook(SERVICE_SENDER));
        } else {
            // sdk 10, sdk 14, sdk 15, sdk 16, sdk 17
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, Intent.class, String.class,
                    new ContextHook(SERVICE_SENDER));
        }
    }

    private void hookActivityManagerServiceBroadcastIntent(Class<?> activityManagerService, ClassLoader classLoader) throws ClassNotFoundException {
        int sdk = Build.VERSION.SDK_INT;
        String method = "broadcastIntent";
        Class<?> iIntentReceiver = Class.forName("android.content.IIntentReceiver", false, classLoader);
        if (sdk >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
            // sdk 18, sdk 19, sdk 21, sdk 22
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, Intent.class, String.class, iIntentReceiver, int.class, String.class, Bundle.class, String.class, int.class, boolean.class, boolean.class, int.class,
                    new IntentContextHook(RECEIVER_SENDER));
        } else if (sdk >= Build.VERSION_CODES.JELLY_BEAN) {
            // sdk 16, sdk 17
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, Intent.class, String.class, iIntentReceiver, int.class, String.class, Bundle.class, String.class, boolean.class, boolean.class, int.class,
                    new IntentContextHook(RECEIVER_SENDER));
        } else {
            // sdk 10, sdk 14, sdk 15
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    IApplicationThread.class, Intent.class, String.class, iIntentReceiver, int.class, String.class, Bundle.class, String.class, boolean.class, boolean.class,
                    new IntentContextHook(RECEIVER_SENDER));
        }
    }

    private void hookActivityManagerServiceStartProcessLocked(Class<?> activityManagerService) {
        int sdk = Build.VERSION.SDK_INT;
        String method = "startProcessLocked";
        if (sdk >= Build.VERSION_CODES.LOLLIPOP) {
            // sdk 21, sdk 22
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    String.class, ApplicationInfo.class, boolean.class, int.class, String.class, ComponentName.class, boolean.class, boolean.class, int.class, boolean.class, String.class, String.class, String[].class, Runnable.class,
                    new ProcessHook());
        } else if (sdk >= Build.VERSION_CODES.KITKAT) {
            // sdk 19, sdk 20
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    String.class, ApplicationInfo.class, boolean.class, int.class, String.class, ComponentName.class, boolean.class, boolean.class, boolean.class,
                    new ProcessHook());
        } else if (sdk >= Build.VERSION_CODES.JELLY_BEAN) {
            // sdk 16, sdk 17, sdk 18
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    String.class, ApplicationInfo.class, boolean.class, int.class, String.class, ComponentName.class, boolean.class, boolean.class,
                    new ProcessHook());
        } else {
            // sdk 10, 14, 15
            XposedHelpers.findAndHookMethod(activityManagerService, method,
                    String.class, ApplicationInfo.class, boolean.class, int.class, String.class, ComponentName.class, boolean.class,
                    new ProcessHook());
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

    private static Object getRecordForAppLocked(Object activityManagerService, Object thread) {
        if (getRecordForAppLocked == null) {
            return null;
        }
        try {
            return getRecordForAppLocked.invoke(activityManagerService, thread);
        } catch (IllegalAccessException e) {
            PreventLog.d("cannot access getRecordForAppLocked", e);
        } catch (InvocationTargetException e) {
            PreventLog.d("cannot invoke getRecordForAppLocked", e);
        }
        return null;
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
            Object processRecord = getRecordForAppLocked(param.thisObject, param.args[0]);
            ApplicationInfo info = ProcessRecordUtils.getInfo(processRecord);
            if (intent != null && intent.hasCategory(Intent.CATEGORY_HOME)) {
                String sender = info == null ? "" : info.packageName;
                PreventLog.v("start activity, intent: " + intent + ", sender: " + sender);
                SystemHook.onStartHomeActivity(sender);
            }
        }
    }

    private static void exportActivityIfNeeded() {
        hookAllMethods(PackageParser.class, "parseActivity", new ExportedActivityHook());
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
            Object processRecord = getRecordForAppLocked(param.thisObject, param.args[0]);
            ApplicationInfo info = ProcessRecordUtils.getInfo(processRecord);
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

    private static class ExportedActivityHook extends XC_MethodHook {
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
    }
}
