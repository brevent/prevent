package me.piebridge.forcestopgb;

import java.lang.reflect.Method;
import java.util.Set;

import android.app.Activity;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Bundle;
import android.os.Process;

import com.saurik.substrate.MS;

public class SubstrateHook {

    public static void initialize() {
        try {
            hookSystemServer$main();
            hookActivity$onCreate();
            hookActivity$onDestroy();
            hookActivity$moveTaskToBack();
            hookActivity$startActivityForResult();
            hookIntentFilter$match();
            hookProcess$killProcess();
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        }
    }

    private static void hookSystemServer$main() {
        MS.hookClassLoad("com.android.server.SystemServer", new MS.ClassLoadHook() {
            @Override
            public void classLoaded(Class<?> SystemServer) {
                try {
                    Method SystemServer$main = SystemServer.getDeclaredMethod("main", String[].class);
                    MS.hookMethod(SystemServer, SystemServer$main, new MS.MethodAlteration<Object, Void>() {
                        @Override
                        public Void invoked(Object thiz, Object... args) throws Throwable {
                            SystemHook.initPackages();
                            return invoke(thiz, args);
                        }
                    });
                } catch (NoSuchMethodException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    private static void hookActivity$onCreate() throws NoSuchMethodException {
        Method Activity$onCreate = Activity.class.getDeclaredMethod("onCreate", Bundle.class);
        MS.hookMethod(Activity.class, Activity$onCreate, new MS.MethodAlteration<Activity, Void>() {
            @Override
            public Void invoked(Activity thiz, Object... args) throws Throwable {
                Hook.beforeActivity$onCreate(thiz, args);
                return invoke(thiz, args);
            }
        });
    }

    private static void hookActivity$onDestroy() throws NoSuchMethodException {
        Method Activity$onDestroy = Activity.class.getDeclaredMethod("onDestroy");
        MS.hookMethod(Activity.class, Activity$onDestroy, new MS.MethodAlteration<Activity, Void>() {
            @Override
            public Void invoked(Activity thiz, Object... args) throws Throwable {
                invoke(thiz, args);
                Hook.afterActivity$onDestroy(thiz, args);
                return null;
            }
        });
    }

    private static void hookActivity$moveTaskToBack() throws NoSuchMethodException {
        Method Activity$moveTaskToBack = Activity.class.getMethod("moveTaskToBack", boolean.class);
        MS.hookMethod(Activity.class, Activity$moveTaskToBack, new MS.MethodAlteration<Activity, Boolean>() {
            @Override
            public Boolean invoked(Activity thiz, Object... args) throws Throwable {
                Boolean result = invoke(thiz, args);
                Hook.afterActivity$moveTaskToBack(thiz, result);
                return result;
            }
        });
    }

    private static void hookActivity$startActivityForResult() throws NoSuchMethodException {
        Method Activity$startActivityForResult = Activity.class.getMethod("startActivityForResult", Intent.class, int.class, Bundle.class);
        MS.hookMethod(Activity.class, Activity$startActivityForResult, new MS.MethodAlteration<Activity, Void>() {
            @Override
            public Void invoked(Activity thiz, Object... args) throws Throwable {
                Intent intent = (Intent) args[0];
                if (intent != null && intent.hasCategory(Intent.CATEGORY_HOME)) {
                    Hook.beforeActivity$startHomeActivityForResult(thiz);
                }
                return invoke(thiz, args);
            }
        });
    }

    private static void hookIntentFilter$match() throws NoSuchMethodException {
        Method IntentFilter$match = IntentFilter.class.getMethod("match", String.class, String.class, String.class, Uri.class, Set.class, String.class);
        MS.hookMethod(IntentFilter.class, IntentFilter$match, new MS.MethodAlteration<IntentFilter, Integer>() {
            @Override
            public Integer invoked(IntentFilter thiz, Object... args) throws Throwable {
                Hook.Result result = Hook.hookIntentFilter$match(thiz, args);
                if (!result.isNone()) {
                    return (Integer) result.getResult();
                } else {
                    return invoke(thiz, args);
                }
            }
        });
    }

    private static void hookProcess$killProcess() throws NoSuchMethodException {
        Method Process$killProcess = Process.class.getMethod("killProcess", int.class);
        MS.hookMethod(Process.class, Process$killProcess, new MS.MethodAlteration<Process, Void>() {
            @Override
            public Void invoked(Process thiz, Object... args) throws Throwable {
                int pid = (Integer) args[0];
                if (Process.myPid() == pid && Hook.stopSelf(pid)) {
                    return null;
                } else {
                    return invoke(thiz, args);
                }
            }
        });
    }

}
