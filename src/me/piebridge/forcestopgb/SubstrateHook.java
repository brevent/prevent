package me.piebridge.forcestopgb;

import java.lang.reflect.Method;
import java.util.Set;

import android.app.Activity;
import android.content.IntentFilter;
import android.net.Uri;
import android.os.Bundle;

import com.saurik.substrate.MS;

public class SubstrateHook extends Hook {

	public static void initialize() {
		hookActivityManagerProxy$forceStopPackage();
		hookActivity$onCreate();
		hookActivity$onDestroy();
		hookIntentFilter$match();
	}

	private static void hookActivityManagerProxy$forceStopPackage() {
		try {
			Class<?> ActivityManagerProxy = Class.forName("android.app.ActivityManagerProxy");
			Method ActivityManagerProxy$forceStopPackage = ActivityManagerProxy.getMethod("forceStopPackage", String.class);
			MS.hookMethod(ActivityManagerProxy, ActivityManagerProxy$forceStopPackage, new MS.MethodAlteration<Object, Void>() {
				@Override
				public Void invoked(Object thiz, Object... args) throws Throwable {
					String packageName = (String) args[0];
					invoke(thiz, packageName);
					afterActivityManagerProxy$forceStopPackage(packageName);
					return null;
				}

			});
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		}
	}

	private static void hookActivity$onCreate() {
		try {
			MS.hookMethod(Activity.class, Activity.class.getDeclaredMethod("onCreate", Bundle.class), new MS.MethodAlteration<Activity, Void>() {
				@Override
				public Void invoked(Activity thiz, Object... args) throws Throwable {
					beforeActivity$onCreate(thiz);
					invoke(thiz, args);
					return null;
				}
			});
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		}
	}

	private static void hookActivity$onDestroy() {
		try {
			MS.hookMethod(Activity.class, Activity.class.getDeclaredMethod("onDestroy"), new MS.MethodAlteration<Activity, Void>() {
				@Override
				public Void invoked(Activity thiz, Object... args) throws Throwable {
					invoke(thiz, args);
					afterActivity$onDestroy(thiz);
					return null;
				}

			});
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		}
	}

	private static void hookIntentFilter$match() {
		try {
			Method IntentFilter$match = IntentFilter.class.getMethod("match", String.class, String.class, String.class, Uri.class, Set.class, String.class);
			MS.hookMethod(IntentFilter.class, IntentFilter$match, new MS.MethodAlteration<IntentFilter, Integer>() {
				@Override
				public Integer invoked(IntentFilter thiz, Object... args) throws Throwable {
					@SuppressWarnings("unchecked")
					Hook.Result result = hookIntentFilter$match(thiz, (String) args[0], (Set<String>) args[4]);
					if (!result.isNone()) {
						return (Integer) result.result;
					} else {
						return invoke(thiz, args);
					}
				}
			});
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		}
	}

}
