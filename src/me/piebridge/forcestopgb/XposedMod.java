package me.piebridge.forcestopgb;

import java.util.Set;

import android.app.Activity;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Bundle;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedHelpers;

public class XposedMod implements IXposedHookZygoteInit {

	@Override
	public void initZygote(IXposedHookZygoteInit.StartupParam startupParam) throws Throwable {
		PreventPackages.ensureDirectory();

		Hook.initPackages();

		// package is force stopped
		XposedHelpers.findAndHookMethod("android.app.ActivityManagerProxy", null, "forceStopPackage", String.class, new XC_MethodHook() {
			@Override
			protected void afterHookedMethod(MethodHookParam param) throws Throwable {
				Hook.afterActivityManagerProxy$forceStopPackage((String) param.args[0]);
			}
		});

		// @formatter:off
		XposedHelpers.findAndHookMethod(IntentFilter.class, "match", String.class, String.class, String.class, Uri.class, Set.class, String.class, new XC_MethodHook() {
			@Override
			protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
				@SuppressWarnings("unchecked")
				Hook.Result result = (Hook.Result) Hook.hookIntentFilter$match((IntentFilter) param.thisObject, (String) param.args[0], (Set<String>) param.args[4]);
				if (!result.isNone()) {
					param.setResult(result.result);
				}
			}
		});
		// @formatter:on

		// dynamic maintain force stopped package
		XposedHelpers.findAndHookMethod(Activity.class, "onCreate", Bundle.class, new XC_MethodHook() {
			@Override
			protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
				Hook.beforeActivity$onCreate((Activity) param.thisObject);
			}
		});
		XposedHelpers.findAndHookMethod(Activity.class, "onDestroy", new XC_MethodHook() {
			@Override
			protected void afterHookedMethod(MethodHookParam param) throws Throwable {
				Hook.afterActivity$onDestroy((Activity) param.thisObject);
			}
		});
	}

}
