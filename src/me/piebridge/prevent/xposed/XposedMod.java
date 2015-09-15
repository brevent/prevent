package me.piebridge.prevent.xposed;

import android.app.Activity;
import android.app.ActivityThread;
import android.appwidget.AppWidgetManager;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageParser;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Process;

import java.util.Set;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XposedHelpers;
import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.ActivityHook;
import me.piebridge.prevent.framework.ActivityManagerServiceHook;
import me.piebridge.prevent.framework.IntentFilterHook;
import me.piebridge.prevent.framework.IntentFilterMatchResult;
import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.framework.SystemHook;
import me.piebridge.prevent.framework.util.LogcatUtils;
import me.piebridge.prevent.framework.util.ProcessRecordUtils;
import me.piebridge.prevent.framework.util.SafeActionUtils;

public class XposedMod implements IXposedHookZygoteInit {

    private static boolean systemHooked;

    public static final ThreadLocal<String> RECEIVER_SENDER = new ThreadLocal<String>();

    public static final ThreadLocal<String> SERVICE_SENDER = new ThreadLocal<String>();

    @Override
    public void initZygote(IXposedHookZygoteInit.StartupParam startupParam) throws Throwable {
        initZygote();
    }

    private static void initZygote() {
        PreventLog.i("prevent running " + BuildConfig.VERSION_NAME);
        XposedBridge.hookAllMethods(ActivityThread.class, "systemMain", new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                if (!systemHooked) {
                    PreventLog.d("start prevent hook (system)");
                    hookSystem(Thread.currentThread().getContextClassLoader());
                    PreventLog.d("finish prevent hook (system)");
                    systemHooked = true;
                    LogcatUtils.logcat();
                }
            }
        });

        PreventLog.d("start prevent hook (zygote)");
        hookActivity();
        hookSuicide();
        hookDestroy();
        PreventLog.d("finish prevent hook (zygote)");
    }

    private static void hookSystem(ClassLoader classLoader) throws ClassNotFoundException, NoSuchMethodException {
        SystemHook.setClassLoader(classLoader);

        Class<?> activityManagerService = Class.forName("com.android.server.am.ActivityManagerService", false, classLoader);

        XposedBridge.hookMethod(ActivityManagerServiceHook.getStartProcessLocked(activityManagerService), new ProcessHook());

        XposedBridge.hookAllMethods(activityManagerService, "broadcastIntent", new IntentContextHook(RECEIVER_SENDER));

        XposedBridge.hookAllMethods(activityManagerService, "startService", new ContextHook(SERVICE_SENDER));
        XposedBridge.hookAllMethods(activityManagerService, "bindService", new ContextHook(SERVICE_SENDER));

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR2) {
            XposedHelpers.findAndHookMethod("android.content.Intent", classLoader, "isExcludingStopped", new IntentExcludingStoppedHook());
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.ICE_CREAM_SANDWICH) {
            XposedBridge.hookMethod(ActivityManagerServiceHook.getCleanUpRemovedTaskLocked(activityManagerService), new CleanUpRemovedHook());
        }

        XposedHelpers.findAndHookMethod("android.content.IntentFilter", classLoader, "match", String.class, String.class, String.class, Uri.class, Set.class, String.class, new IntentFilterMatchHook());

        XposedBridge.hookAllMethods(activityManagerService, "removeProcessLocked", new IgnoreDependencyHook());

        if (BuildConfig.ALIPAY_DONATE || BuildConfig.WECHAT_DONATE) {
            exportActivityIfNeeded();
        }
    }

    private static void exportActivityIfNeeded() {
        XposedBridge.hookAllMethods(PackageParser.class, "parseActivity", new XC_MethodHook() {
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

    private static void hookActivity() {
        XposedHelpers.findAndHookMethod(Activity.class, "onCreate", Bundle.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                ActivityHook.hookBeforeOnCreate((Activity) param.thisObject);
            }
        });

        XposedHelpers.findAndHookMethod(Activity.class, "onDestroy", new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                ActivityHook.hookAfterOnDestroy((Activity) param.thisObject);
            }
        });

        XposedHelpers.findAndHookMethod(Activity.class, "onRestart", new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                ActivityHook.hookBeforeOnRestart((Activity) param.thisObject);
            }
        });
    }

    private static void hookSuicide() {
        XposedHelpers.findAndHookMethod(Process.class, "killProcess", int.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                int pid = (Integer) param.args[0];
                if (Process.myPid() == pid) {
                    ActivityHook.stopSelf(pid);
                }
            }
        });

        XposedHelpers.findAndHookMethod(System.class, "exit", int.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                ActivityHook.stopSelf(-1);
            }
        });
    }

    private static void hookDestroy() {
        XposedHelpers.findAndHookMethod(Activity.class, "moveTaskToBack", boolean.class, new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                ActivityHook.hookAfterMoveTaskToBack((Activity) param.thisObject, (Boolean) param.getResult());
            }
        });

        XC_MethodHook hookStartActivityForResult = new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                Intent intent = (Intent) param.args[0];
                if (intent != null && intent.hasCategory(Intent.CATEGORY_HOME)) {
                    ActivityHook.hookBeforeStartHomeActivityForResult((Activity) param.thisObject);
                }
            }
        };

        if (Build.VERSION.SDK_INT > Build.VERSION_CODES.ICE_CREAM_SANDWICH_MR1) {
            XposedHelpers.findAndHookMethod(Activity.class, "startActivityForResult", Intent.class, int.class, Bundle.class, hookStartActivityForResult);
        } else {
            XposedHelpers.findAndHookMethod(Activity.class, "startActivityForResult", Intent.class, int.class, hookStartActivityForResult);
        }
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
                IntentFilterMatchResult result = IntentFilterHook.hookAfterMatch(param.thisObject, param.args);
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
