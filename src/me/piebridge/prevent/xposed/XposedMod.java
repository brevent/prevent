package me.piebridge.prevent.xposed;

import android.app.Activity;
import android.app.ActivityThread;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageParser;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Process;

import java.lang.reflect.Method;
import java.util.Set;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedBridge;
import de.robv.android.xposed.XposedHelpers;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.ActivityHook;
import me.piebridge.prevent.framework.ActivityManagerServiceHook;
import me.piebridge.prevent.framework.IntentFilterHook;
import me.piebridge.prevent.framework.IntentFilterMatchResult;
import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.framework.SystemHook;

public class XposedMod implements IXposedHookZygoteInit {

    private static boolean systemHooked;

    @Override
    public void initZygote(IXposedHookZygoteInit.StartupParam startupParam) throws Throwable {
        initZygote();
    }

    private static void initZygote() {
        XposedBridge.hookAllMethods(ActivityThread.class, "systemMain", new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                if (!systemHooked) {
                    PreventLog.d("start prevent hook (system)");
                    hookSystem(Thread.currentThread().getContextClassLoader());
                    PreventLog.d("finish prevent hook (system)");
                    systemHooked = true;
                }
            }
        });

        PreventLog.d("start prevent hook (normal)");
        hookActivity();
        hookSuicide();
        hookDestroy();
        PreventLog.d("finish prevent hook (normal)");
    }

    private static void hookSystem(ClassLoader classLoader) throws ClassNotFoundException, NoSuchMethodException {
        SystemHook.setClassLoader(classLoader);

        Class<?> activityManagerService = Class.forName("com.android.server.am.ActivityManagerService", false, classLoader);
        Method startProcessLocked = ActivityManagerServiceHook.getStartProcessLocked(activityManagerService);
        XposedBridge.hookMethod(startProcessLocked, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                if (!ActivityManagerServiceHook.hookBeforeStartProcessLocked(param.thisObject, param.args)) {
                    param.setResult(null);
                }
            }
        });

        Method cleanUpRemovedTaskLocked = ActivityManagerServiceHook.getCleanUpRemovedTaskLocked(activityManagerService);
        if (cleanUpRemovedTaskLocked != null) {
            XposedBridge.hookMethod(cleanUpRemovedTaskLocked, new XC_MethodHook() {
                @Override
                protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                    ActivityManagerServiceHook.hookAfterCleanUpRemovedTaskLocked(param.args);
                }
            });
        }

        final Class<?> intentFilter = Class.forName("android.content.IntentFilter", false, classLoader);
        Method match = intentFilter.getMethod("match", String.class, String.class, String.class, Uri.class, Set.class, String.class);
        XposedBridge.hookMethod(match, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                if (IntentFilterHook.canHook()) {
                    IntentFilterMatchResult result = IntentFilterHook.hookBeforeMatch(param.thisObject, param.args);
                    if (!result.isNone()) {
                        param.setResult(result.getResult());
                    }
                }
            }
        });

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

}
