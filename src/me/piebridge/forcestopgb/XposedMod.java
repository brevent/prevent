package me.piebridge.forcestopgb;

import java.util.Set;

import android.app.Activity;
import android.content.Intent;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Bundle;
import android.os.Process;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedHelpers;

public class XposedMod implements IXposedHookZygoteInit {

    @Override
    public void initZygote(IXposedHookZygoteInit.StartupParam startupParam) throws Throwable {
        Hook.initPackages();

        // dynamic maintain force stopped package
        XposedHelpers.findAndHookMethod(Activity.class, "onCreate", Bundle.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                Hook.beforeActivity$onCreate((Activity) param.thisObject, param.args);
            }
        });

        XposedHelpers.findAndHookMethod(Activity.class, "onDestroy", new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                Hook.afterActivity$onDestroy((Activity) param.thisObject, param.args);
            }
        });

        // @formatter:off
        XposedHelpers.findAndHookMethod(IntentFilter.class, "match", String.class, String.class, String.class, Uri.class, Set.class, String.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                Hook.Result result = (Hook.Result) Hook.hookIntentFilter$match((IntentFilter) param.thisObject, param.args);
                if (!result.isNone()) {
                    param.setResult(result.getResult());
                }
            }
        });
        // @formatter:on

        XposedHelpers.findAndHookMethod(Process.class, "killProcess", int.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                int pid = (Integer) param.args[0];
                if (Process.myPid() == pid && Hook.stopSelf(pid)) {
                    param.setResult(null);
                }
            }
        });

        XposedHelpers.findAndHookMethod(Activity.class, "moveTaskToBack", boolean.class, new XC_MethodHook() {
            @Override
            protected void afterHookedMethod(MethodHookParam param) throws Throwable {
                Hook.afterActivity$moveTaskToBack((Activity) param.thisObject, (Boolean) param.getResult());
            }
        });

        XposedHelpers.findAndHookMethod(Activity.class, "startActivity", Intent.class, new XC_MethodHook() {
            @Override
            protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
                Intent intent = (Intent) param.args[0];
                if (intent != null && intent.hasCategory(Intent.CATEGORY_HOME)) {
                    param.setResult(null);
                    Hook.forceStopActivity((Activity) param.thisObject);
                }
            }
        });
    }

}
