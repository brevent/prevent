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
		hookSystemServer$main();
		hookActivity$onCreate();
		hookActivity$onDestroy();
		hookIntentFilter$match();
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
							Hook.initPackages();
							return invoke(thiz, args);
						}
					});
				} catch (NoSuchMethodException e) {
					e.printStackTrace();
				}
			}
		});
	}

	private static void hookActivity$onCreate() {
		try {
			Method Activity$onCreate = Activity.class.getDeclaredMethod("onCreate", Bundle.class);
			MS.hookMethod(Activity.class, Activity$onCreate, new MS.MethodAlteration<Activity, Void>() {
				@Override
				public Void invoked(Activity thiz, Object... args) throws Throwable {
					beforeActivity$onCreate(thiz);
					invoke(thiz, args);
					return null;
				}
			});
		} catch (NoSuchMethodException e) {
			throw new NoSuchMethodError(e.getMessage());
		}
	}

	private static void hookActivity$onDestroy() {
		try {
			Method Activity$onDestroy = Activity.class.getDeclaredMethod("onDestroy");
			MS.hookMethod(Activity.class, Activity$onDestroy, new MS.MethodAlteration<Activity, Void>() {
				@Override
				public Void invoked(Activity thiz, Object... args) throws Throwable {
					invoke(thiz, args);
					afterActivity$onDestroy(thiz);
					return null;
				}
			});
		} catch (NoSuchMethodException e) {
			throw new NoSuchMethodError(e.getMessage());
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
			throw new NoSuchMethodError(e.getMessage());
		}
	}

}
