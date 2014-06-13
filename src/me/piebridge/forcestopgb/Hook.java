package me.piebridge.forcestopgb;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import android.app.Activity;
import android.appwidget.AppWidgetManager;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.view.inputmethod.InputMethod;

public class Hook {

	public static final String TAG = Hook.class.getPackage().getName();

	private static final String ACTION_HOOK = TAG + ".HOOK";

	public static final String ACTION_XPOSED_SECTION = "de.robv.android.xposed.installer.OPEN_SECTION";

	public static final int FLAG_ACTIVITY_LAUNCHER = Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_RESET_TASK_IF_NEEDED;

	private static long lastModified;

	private static Map<String, Boolean> preventPackages;

	private static Map<String, Boolean> systemPackages = new HashMap<String, Boolean>();

	private static void reloadPackagesIfNeeded() {
		long time = PreventPackages.lastModified();
		if (time > lastModified || preventPackages == null) {
			lastModified = time;
			preventPackages = PreventPackages.load();
		}
	}

	private static void savePackages(String suffix) {
		lastModified = PreventPackages.save(preventPackages, suffix);
	}

	private static Field getField(Class<?> clazz, boolean canPrivate, String name) {
		if (clazz == null) {
			return null;
		}
		try {
			final Field field = clazz.getDeclaredField(name);
			boolean isPrivate = Modifier.isPrivate(field.getModifiers());
			if (!isPrivate) {
				return field;
			}
			if (isPrivate && canPrivate) {
				field.setAccessible(true);
				return field;
			}
			if (canPrivate) {
				return clazz.getField(name);
			}
		} catch (NoSuchFieldException e) {
		}
		return getField(clazz.getSuperclass(), false, name);
	}

	private static Field getField(Class<?> clazz, String name) {
		return getField(clazz, true, name);
	}

	private static Field getField(Class<?> clazz, String... names) {
		for (String name : names) {
			final Field field = getField(clazz, name);
			if (field != null) {
				return field;
			}
		}
		return null;
	}

	private static Object getObjectField(Object object, String name) {
		Field field = getField(object.getClass(), name);
		if (field != null) {
			try {
				return field.get(object);
			} catch (IllegalAccessException e) {
				throw new IllegalAccessError(e.getMessage());
			}
		}
		return null;
	}

	private static boolean hasLauncher(ArrayList<?> activities) {
		for (Object activity : activities) {
			@SuppressWarnings("unchecked")
			ArrayList<? extends IntentFilter> intents = (ArrayList<? extends IntentFilter>) getObjectField(activity, "intents");
			for (IntentFilter intent : intents) {
				if (intent.hasAction(Intent.ACTION_MAIN) && (intent.hasCategory(Intent.CATEGORY_INFO) || intent.hasCategory(Intent.CATEGORY_LAUNCHER))) {
					android.util.Log.d(TAG, "activity " + activity + " is launcher");
					return true;
				}
			}
		}
		return false;
	}

	private static ThreadLocal<Integer> count = new ThreadLocal<Integer>() {
		@Override
		protected Integer initialValue() {
			return 0;
		}
	};

	public static void initPackages() {
		Map<String, Boolean> packages = PreventPackages.load();
		if (packages.containsValue(Boolean.FALSE)) {
			for (String key : packages.keySet()) {
				if (!packages.get(key)) {
					packages.put(key, Boolean.TRUE);
				}
			}
			PreventPackages.save(packages, "initPackages");
		}
	}

	public static void beforeActivity$onCreate(Activity thiz, Object... args) {
		Intent intent = thiz.getIntent();
		android.util.Log.d(TAG, "before onCreate: " + intent + ", count: " + count.get());
		count.set(count.get() + 1);
		reloadPackagesIfNeeded();
		String packageName = intent.getComponent().getPackageName();
		if (!preventPackages.containsKey(packageName)) {
			return;
		}
		if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
			preventPackages.put(packageName, Boolean.FALSE);
			savePackages("beforeActivity$onCreate");
		}
	}

	public static void afterActivity$onDestroy(Activity thiz, Object... args) {
		count.set(count.get() - 1);
		Intent intent = thiz.getIntent();
		android.util.Log.d(TAG, "after onDestroy: " + intent + ", count: " + count.get());
		reloadPackagesIfNeeded();
		String packageName = intent.getComponent().getPackageName();
		if (!preventPackages.containsKey(packageName)) {
			return;
		}
		if (count.get() == 0 && Boolean.FALSE.equals(preventPackages.get(packageName))) {
			preventPackages.put(packageName, Boolean.TRUE);
			savePackages("afterActivity$onDestroy");
		}
	}

	public static Result hookIntentFilter$match(IntentFilter thiz, Object... args) {
		String action = (String) args[0];
		if (ACTION_HOOK.equals(action)) {
			PreventPackages.ensureDirectory();
			return new Result(int.class, -IntentFilter.NO_MATCH_ACTION);
		}
		if (InputMethod.SERVICE_INTERFACE.equals(action)) {
			// input method
			return Result.None;
		} else if (AppWidgetManager.ACTION_APPWIDGET_UPDATE.equals(action)) {
			// app widget
			return Result.None;
		}

		// thiz.activity or thiz.service or thiz.provider
		Field field = getField(thiz.getClass(), "service", "activity", "provider");
		if (field == null) {
			return Result.None;
		}

		Object component;
		try {
			component = field.get(thiz);
		} catch (IllegalAccessException e) {
			return Result.None;
		}

		// component.owner
		Object owner = getObjectField(component, "owner");

		// component.owner.packageName
		String packageName = (String) getObjectField(owner, "packageName");

		// component.owner.applicationInfo
		ApplicationInfo applicationInfo = (ApplicationInfo) getObjectField(owner, "applicationInfo");

		// if the application is disabled, we do nothing
		if (!applicationInfo.enabled) {
			return Result.None;
		}

		// component.owner.providers
		ArrayList<?> providers = (ArrayList<?>) getObjectField(owner, "providers");
		if (providers.contains(component)) {
			// we don't filter providers
			return Result.None;
		}

		// component.owner.activities
		ArrayList<?> activities = (ArrayList<?>) getObjectField(owner, "activities");
		if ((applicationInfo.flags & (ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP)) != 0) {
			if (systemPackages.get(packageName) == null) {
				systemPackages.put(packageName, hasLauncher(activities));
			}
		}
		if (activities.contains(component)) {
			// we don't filter activities
			return Result.None;
		}

		if (Boolean.FALSE.equals(systemPackages.get(packageName))) {
			// we don't filter system package has no launcher
			return Result.None;
		}

		reloadPackagesIfNeeded();
		if (Boolean.TRUE.equals(preventPackages.get(packageName))) {
			android.util.Log.d(TAG, "disallow " + packageName + ", action: " + action);
			return new Result(int.class, IntentFilter.NO_MATCH_ACTION);
		}
		return Result.None;
	}

	public static class Result {
		public static final Result None = new Result();
		public Class<?> type;
		public Object result;

		private Result() {
			type = Void.class;
		}

		public Result(Object _result) {
			result = _result;
			if (result != null) {
				type = _result.getClass();
			}
		}

		public Result(Class<?> _type, Object _result) {
			type = _type;
			result = _result;
		}

		public boolean isNone() {
			return Void.class.equals(this.type);
		}
	}

	public static boolean isHookEnabled() {
		return new IntentFilter().match(ACTION_HOOK, null, null, null, null, null) == -IntentFilter.NO_MATCH_ACTION;
	}

}
