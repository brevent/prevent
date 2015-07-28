package me.piebridge.prevent.xposed;

import android.app.Activity;
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
import me.piebridge.prevent.framework.Hook;
import me.piebridge.prevent.framework.IntentFilterMatchResult;
import me.piebridge.prevent.framework.PreventLog;
import me.piebridge.prevent.framework.SystemHook;

public class XposedMod implements IXposedHookZygoteInit {

    @Override
    public void initZygote(IXposedHookZygoteInit.StartupParam startupParam) throws Throwable {
        initZygote();
    }

    private static void initZygote() throws Throwable { // NOSONAR
        PreventLog.d("start prevent hook");
        Class<?> ActivityThread = Class.forName("android.app.ActivityThread"); // NOSONAR
        XposedBridge.hookAllMethods(ActivityThread, "systemMain", new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                hookSystem(Thread.currentThread().getContextClassLoader());
            }
        });

        hookActivity();
        hookSuicide();
        hookDestroy();
        PreventLog.d("finish prevent hook");
    }

    private static void hookSystem(ClassLoader classLoader) throws ClassNotFoundException, NoSuchMethodException {
        SystemHook.setClassLoader(classLoader);

        Class<?> ActivityManagerService = Class.forName("com.android.server.am.ActivityManagerService", false, classLoader); // NOSONAR
        Method startProcessLocked = SystemHook.getStartProcessLocked(ActivityManagerService);
        XposedBridge.hookMethod(startProcessLocked, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                if (!SystemHook.beforeActivityManagerService$startProcessLocked(param.args)) {
                    param.setResult(null);
                }
            }
        });

        Method cleanUpRemovedTaskLocked = SystemHook.getCleanUpRemovedTaskLocked(ActivityManagerService);
        if (cleanUpRemovedTaskLocked != null) {
            XposedBridge.hookMethod(cleanUpRemovedTaskLocked, new XC_MethodHook() {
                @Override
                protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                    SystemHook.afterActivityManagerService$cleanUpRemovedTaskLocked(param.args);
                }
            });
        }

        Class<?> IntentFilter = Class.forName("android.content.IntentFilter", false, classLoader); // NOSONAR
        Method match = IntentFilter.getMethod("match", String.class, String.class, String.class, Uri.class, Set.class, String.class);
        XposedBridge.hookMethod(match, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                IntentFilterMatchResult result = SystemHook.hookIntentFilter$match(param.thisObject, param.args);
                if (!result.isNone()) {
                    param.setResult(result.getResult());
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
                Hook.beforeActivity$onCreate((Activity) param.thisObject);
            }
        });

        XposedHelpers.findAndHookMethod(Activity.class, "onDestroy", new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                Hook.afterActivity$onDestroy((Activity) param.thisObject);
            }
        });

        XposedHelpers.findAndHookMethod(Activity.class, "onRestart", new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                Hook.beforeActivity$onRestart((Activity) param.thisObject);
            }
        });
    }

    private static void hookSuicide() {
        XposedHelpers.findAndHookMethod(Process.class, "killProcess", int.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                int pid = (Integer) param.args[0];
                if (Process.myPid() == pid) {
                    Hook.stopSelf(pid);
                }
            }
        });

        XposedHelpers.findAndHookMethod(System.class, "exit", int.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                Hook.stopSelf(-1);
            }
        });
    }

    private static void hookDestroy() {
        XposedHelpers.findAndHookMethod(Activity.class, "moveTaskToBack", boolean.class, new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                Hook.afterActivity$moveTaskToBack((Activity) param.thisObject, (Boolean) param.getResult());
            }
        });

        XC_MethodHook hookStartActivityForResult = new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                Intent intent = (Intent) param.args[0];
                if (intent != null && intent.hasCategory(Intent.CATEGORY_HOME)) {
                    Hook.beforeActivity$startHomeActivityForResult((Activity) param.thisObject);
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
