package me.piebridge.forcestopgb;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import android.app.Activity;
import android.appwidget.AppWidgetManager;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.net.Uri;
import android.os.Bundle;
import android.view.inputmethod.InputMethod;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedHelpers;

public class XposedMod implements IXposedHookZygoteInit {

	public static final String TAG = "me.piebridge.forcestopgb";

	abstract class MethodHook extends XC_MethodHook {
		private long mtime;
		protected Map<String, Boolean> packages;

		protected MethodHook() {
			loadPackages();
		}

		protected void loadPackages() {
			mtime = PackageProvider.getMTime(PackageProvider.FORCESTOP);
			packages = PackageProvider.loadFromFile(PackageProvider.FORCESTOP);
		}

		protected void reloadPackagesIfNeeded() {
			long time = PackageProvider.getMTime(PackageProvider.FORCESTOP);
			if (time > mtime) {
				packages = PackageProvider.loadFromFile(PackageProvider.FORCESTOP);
				mtime = time;
			}
		}

		protected void savePackages(String suffix) {
			mtime = PackageProvider.saveToFile(PackageProvider.FORCESTOP, packages, suffix);
		}

		protected Field scanField(Class<?> clazz, String... names) {
			for (String name : names) {
				Field field;
				try {
					field = clazz.getDeclaredField(name);
					field.setAccessible(true);
					return field;
				} catch (NoSuchFieldException e) {
				}
				try {
					field = clazz.getField(name);
					return field;
				} catch (NoSuchFieldException e) {
				}
			}
			return null;
		}
	}

	ThreadLocal<Integer> count = new ThreadLocal<Integer>() {
		@Override
		protected Integer initialValue() {
			return 0;
		}
	};

	class Hook_Activity_onCreate extends MethodHook {
		@Override
		protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
			Activity activity = (Activity) param.thisObject;
			Intent intent = activity.getIntent();
			String packageName = intent.getComponent().getPackageName();
			Set<String> categories = intent.getCategories();
			int oldCount = count.get();
			android.util.Log.d(TAG, "onCreate: " + intent + ", count: " + oldCount);
			if (intent.getSourceBounds() != null && Intent.ACTION_MAIN.equals(intent.getAction())
					&& (categories != null && categories.contains(Intent.CATEGORY_LAUNCHER))) {
				count.set(1);
			} else {
				if (oldCount < 0) {
					oldCount = 0;
				}
				count.set(oldCount + 1);
			}
			reloadPackagesIfNeeded();
			if (!packages.containsKey(packageName)) {
				return;
			}
			if (Boolean.TRUE.equals(packages.get(packageName))) {
				packages.put(packageName, Boolean.FALSE);
				savePackages("Hook_Activity_onCreate");
			}
		}
	}

	class Hook_Activity_onDestroy extends MethodHook {
		@Override
		protected void afterHookedMethod(MethodHookParam param) throws Throwable {
			Activity activity = (Activity) param.thisObject;
			Intent intent = activity.getIntent();
			String packageName = intent.getComponent().getPackageName();
			int oldCount = count.get();
			android.util.Log.d(TAG, "onDestroy: " + intent + ", count: " + oldCount);
			if (oldCount > 1) {
				count.set(oldCount - 1);
			} else {
				count.set(0);
			}
			reloadPackagesIfNeeded();
			if (!packages.containsKey(packageName)) {
				return;
			}
			if (count.get() == 0 && Boolean.FALSE.equals(packages.get(packageName))) {
				packages.put(packageName, Boolean.TRUE);
				savePackages("Hook_Activity_onDestroy");
			}
		}
	}

	class Hook_ActivityManagerProxy_forceStopPackage extends MethodHook {
		@Override
		protected void afterHookedMethod(MethodHookParam param) throws Throwable {
			String packageName = (String) param.args[0];
			reloadPackagesIfNeeded();
			if (!Boolean.TRUE.equals(packages.get(packageName))) {
				packages.put(packageName, Boolean.TRUE);
				savePackages("Hook_ActivityManagerProxy_forceStopPackage");
			}
			android.util.Log.d(TAG, "forceStopPackage: " + packageName);
		}
	}

	class Hook_IntentFilter_match extends MethodHook {
		private final Map<String, Boolean> systemPackages;
		private final Set<String> standardActivityActions;

		public Hook_IntentFilter_match() {
			super();
			systemPackages = new HashMap<String, Boolean>();
			standardActivityActions = new HashSet<String>();
			standardActivityActions.add(Intent.ACTION_MAIN);
			standardActivityActions.add(Intent.ACTION_VIEW);
			standardActivityActions.add(Intent.ACTION_ATTACH_DATA);
			standardActivityActions.add(Intent.ACTION_EDIT);
			standardActivityActions.add(Intent.ACTION_PICK);
			standardActivityActions.add(Intent.ACTION_CHOOSER);
			standardActivityActions.add(Intent.ACTION_GET_CONTENT);
			standardActivityActions.add(Intent.ACTION_DIAL);
			standardActivityActions.add(Intent.ACTION_CALL);
			standardActivityActions.add(Intent.ACTION_SEND);
			standardActivityActions.add(Intent.ACTION_SENDTO);
			standardActivityActions.add(Intent.ACTION_ANSWER);
			standardActivityActions.add(Intent.ACTION_INSERT);
			standardActivityActions.add(Intent.ACTION_DELETE);
			standardActivityActions.add(Intent.ACTION_RUN);
			standardActivityActions.add(Intent.ACTION_SYNC);
			standardActivityActions.add(Intent.ACTION_PICK_ACTIVITY);
			standardActivityActions.add(Intent.ACTION_SEARCH);
			standardActivityActions.add(Intent.ACTION_WEB_SEARCH);
			standardActivityActions.add(Intent.ACTION_FACTORY_TEST);
		}

		@Override
		protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
			String action = (String) param.args[0];
			@SuppressWarnings("unchecked")
			Set<String> categories = (Set<String>) param.args[4];
			if (InputMethod.SERVICE_INTERFACE.equals(action)) {
				// input method
				return;
			} else if (AppWidgetManager.ACTION_APPWIDGET_UPDATE.equals(action)) {
				// app widget
				return;
			}

			String packageName = null;
			boolean isSystemPackage = false;
			// param.thisObject.activity or param.thisObject.service
			Field field = scanField(param.thisObject.getClass(), "service", "activity");
			if (field == null) {
				return;
			}

			try {
				Object object = field.get(param.thisObject);
				// object.owner.packageName
				Object owner = XposedHelpers.getObjectField(object, "owner");
				packageName = (String) XposedHelpers.getObjectField(owner, "packageName");
				// object.owner.applicationInfo
				ApplicationInfo applicationInfo = (ApplicationInfo) XposedHelpers.getObjectField(owner, "applicationInfo");
				if ((applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0) {
					isSystemPackage = true;
				}
			} catch (Throwable t) {
				t.printStackTrace();
			}
			if (packageName == null) {
				return;
			}
			if (isSystemPackage && !systemPackages.containsKey(packageName)) {
				systemPackages.put(packageName, Boolean.FALSE);
			}

			if (Intent.ACTION_MAIN.equals(action) && categories != null && categories.contains(Intent.CATEGORY_LAUNCHER)) {
				// launcher
				if (Boolean.FALSE.equals(systemPackages.get(packageName))) {
					android.util.Log.d(TAG, "system launcher package " + packageName);
					systemPackages.put(packageName, Boolean.TRUE);
				}
				return;
			}

			if (standardActivityActions.contains(action)) {
				return;
			}

			reloadPackagesIfNeeded();
			if (Boolean.TRUE.equals(packages.get(packageName)) && !Boolean.FALSE.equals(systemPackages.get(packageName))) {
				param.setResult(IntentFilter.NO_MATCH_ACTION);
			}
		}
	}

	@Override
	public void initZygote(IXposedHookZygoteInit.StartupParam startupParam) throws Throwable {
		PackageProvider.ensureDirectory();

		// package is force stopped
		XposedHelpers.findAndHookMethod("android.app.ActivityManagerProxy", null, "forceStopPackage", String.class,
				new Hook_ActivityManagerProxy_forceStopPackage());

		XposedHelpers.findAndHookMethod(IntentFilter.class, "match", String.class, String.class, String.class, Uri.class, Set.class, String.class,
				new Hook_IntentFilter_match());

		// dynamic maintain force stopped package
		XposedHelpers.findAndHookMethod(Activity.class, "onCreate", Bundle.class, new Hook_Activity_onCreate());
		XposedHelpers.findAndHookMethod(Activity.class, "onDestroy", new Hook_Activity_onDestroy());
	}

}
