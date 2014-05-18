package me.piebridge.forcestopgb;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.appwidget.AppWidgetManager;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.FileUtils;
import android.os.IBinder;
import android.os.ParcelFileDescriptor;
import android.view.inputmethod.InputMethod;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedHelpers;

public class XposedMod implements IXposedHookZygoteInit {

	public static final String TAG = "me.piebridge.forcestopgb";
	@SuppressLint("SdCardPath")
	private static final String FORCESTOP = "/data/data/me.piebridge.forcestopgb/conf/forcestop.list";

	abstract class MethodHook extends XC_MethodHook {
		private long mtime;
		protected Map<String, Boolean> packages;

		protected MethodHook() {
			loadPackages();
		}

		protected void loadPackages() {
			mtime = getMTime(FORCESTOP);
			packages = loadFromFile(FORCESTOP);
		}

		protected void reloadPackagesIfNeeded() {
			long time = getMTime(FORCESTOP);
			if (time > mtime) {
				packages = loadFromFile(FORCESTOP);
				mtime = time;
			}
		}

		protected void savePackages(String suffix) {
			saveToFile(FORCESTOP, packages, suffix);
			mtime = getMTime(FORCESTOP);
		}

		protected void saveToFile(String path, Map<String, Boolean> packages, String suffix) {
			try {
				File file = new File(path + suffix);
				BufferedWriter writer = new BufferedWriter(new FileWriter(file));
				for (Entry<String, Boolean> entry : packages.entrySet()) {
					writer.write(entry.getKey());
					writer.write("=");
					writer.write(String.valueOf(entry.getValue()));
					writer.write("\n");
				}
				writer.close();
				FileUtils.setPermissions(file.getAbsolutePath(), 0666, -1, -1);
				file.renameTo(new File(path));
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		protected Map<String, Boolean> loadFromFile(String path) {
			Map<String, Boolean> packages = new TreeMap<String, Boolean>();
			try {
				String line;
				File file = new File(path);
				BufferedReader reader = new BufferedReader(new FileReader(file));
				while ((line = reader.readLine()) != null) {
					line = line.trim();
					String[] components = line.split("=");
					if (components.length > 1) {
						packages.put(components[0], Boolean.valueOf(components[1]));
					} else {
						packages.put(line, Boolean.TRUE);
					}
				}
				reader.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
			return packages;
		}

		protected long getMTime(String path) {
			File file = new File(path);
			if (file != null && file.exists()) {
				return file.lastModified();
			}
			return 0L;
		}
	}

	class Hook_ActivityManagerProxy_startActivity extends MethodHook {
		private int index;

		Hook_ActivityManagerProxy_startActivity(int index) {
			super();
			this.index = index;
		}

		@Override
		protected void afterHookedMethod(MethodHookParam param) throws Throwable {
			Intent intent = (Intent) param.args[index];
			if (intent == null || intent.getComponent() == null) {
				return;
			}
			if (!Intent.ACTION_MAIN.equals(intent.getAction())) {
				return;
			}
			Set<String> categories = intent.getCategories();
			if (categories == null || !categories.contains(Intent.CATEGORY_LAUNCHER)) {
				return;
			}
			String packageName = intent.getComponent().getPackageName();
			reloadPackagesIfNeeded();
			if (Boolean.TRUE.equals(packages.get(packageName))) {
				packages.put(packageName, Boolean.FALSE);
				savePackages("Hook_ActivityManagerProxy_startActivity");
				android.util.Log.d(TAG, "start package " + packageName);
			}
		}
	}

	class Hook_Activity_finish extends MethodHook {
		private Field mParentField = null;

		public Hook_Activity_finish() {
			super();
			try {
				mParentField = Activity.class.getDeclaredField("mParent");
				mParentField.setAccessible(true);
			} catch (NoSuchFieldException e) {
				// do nothing
			}
		}

		@Override
		protected void afterHookedMethod(MethodHookParam param) throws Throwable {
			if (mParentField != null && mParentField.get(param.thisObject) != null) {
				return;
			}
			Activity activity = (Activity) param.thisObject;
			String packageName = activity.getApplicationInfo().packageName;
			reloadPackagesIfNeeded();
			if (Boolean.FALSE.equals(packages.get(packageName))) {
				packages.put(packageName, Boolean.TRUE);
				savePackages("Hook_Activity_finish");
				android.util.Log.d(TAG, "finish package " + packageName);
			}
		}
	}

	class Hook_ActivityManagerProxy_forceStopPackage extends MethodHook {
		public Hook_ActivityManagerProxy_forceStopPackage() {
			super();
		}

		@Override
		protected void afterHookedMethod(MethodHookParam param) throws Throwable {
			String packageName = (String) param.args[0];
			reloadPackagesIfNeeded();
			if (!Boolean.TRUE.equals(packages.get(packageName))) {
				packages.put(packageName, Boolean.TRUE);
				savePackages("Hook_ActivityManagerProxy_forceStopPackage");
			}
			android.util.Log.d(TAG, "forcestop package " + packageName);
		}
	}

	class Hook_IntentFilter_match extends MethodHook {
		private Map<String, Boolean> systemPackages;;

		public Hook_IntentFilter_match() {
			super();
			systemPackages = new HashMap<String, Boolean>();
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

			Object object = null;
			String packageName = null;
			boolean isSystemPackage = false;
			try {
				// param.thisObject.activity
				object = param.thisObject.getClass().getField("activity").get(param.thisObject);
			} catch (NoSuchFieldException e) {
			}
			try {
				// param.thisObject.service
				object = param.thisObject.getClass().getField("service").get(param.thisObject);
			} catch (NoSuchFieldException e) {
			}
			if (object != null) {
				// object.owner.packageName
				Object owner = object.getClass().getField("owner").get(object);
				packageName = (String) owner.getClass().getField("packageName").get(owner);

				try {
					// object.owner.applicationInfo
					ApplicationInfo applicationInfo = (ApplicationInfo) owner.getClass().getField("applicationInfo").get(owner);
					if ((applicationInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0) {
						isSystemPackage = true;
					}
				} catch (Throwable t) {
					t.printStackTrace();
				}
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

			if (isSystemPackage && Intent.ACTION_VIEW.equals(action)) {
				// other wise, there is no way to open system's default action
				return;
			}

			reloadPackagesIfNeeded();
			if (Boolean.TRUE.equals(packages.get(packageName)) && !Boolean.FALSE.equals(systemPackages.get(packageName))) {
				param.setResult(IntentFilter.NO_MATCH_ACTION);
				android.util.Log.d(TAG, "ignore intent-filter for: " + packageName + ", action: " + action +
					", categories: " + Arrays.toString(categories == null ? null : categories.toArray()));
			}
		}
	}


	@Override
	public void initZygote(IXposedHookZygoteInit.StartupParam startupParam) throws Throwable {
		Class<?> ActivityManagerProxy = Class.forName("android.app.ActivityManagerProxy");

		File parent = new File(FORCESTOP).getParentFile();
		parent.mkdirs();
		FileUtils.setPermissions(parent.getAbsolutePath(), 0777, 1000, 1000);

		if (Build.VERSION.SDK_INT < 14) {
			XposedHelpers.findAndHookMethod(ActivityManagerProxy, "startActivity", "android.app.IApplicationThread", Intent.class,
				String.class, Uri[].class, int.class, IBinder.class, String.class,
				int.class, boolean.class, boolean.class, new Hook_ActivityManagerProxy_startActivity(1));
		} else if (Build.VERSION.SDK_INT < 16) {
			XposedHelpers.findAndHookMethod(ActivityManagerProxy, "startActivity", "android.app.IApplicationThread", Intent.class,
				String.class, Uri[].class, int.class, IBinder.class, String.class, int.class, boolean.class,
				boolean.class, String.class, ParcelFileDescriptor.class, boolean.class, new Hook_ActivityManagerProxy_startActivity(1));
		} else if (Build.VERSION.SDK_INT < 18){
			XposedHelpers.findAndHookMethod(ActivityManagerProxy, "startActivity", "android.app.IApplicationThread", Intent.class,
				String.class, IBinder.class, String.class, int.class, int.class, String.class,
				ParcelFileDescriptor.class, Bundle.class, new Hook_ActivityManagerProxy_startActivity(1));
		} else {
			XposedHelpers.findAndHookMethod(ActivityManagerProxy, "startActivity", "android.app.IApplicationThread", String.class, Intent.class,
				String.class, IBinder.class, String.class, int.class,
				int.class, String.class, ParcelFileDescriptor.class, Bundle.class, new Hook_ActivityManagerProxy_startActivity(2));
		}

		XposedHelpers.findAndHookMethod(Activity.class, "finish", new Hook_Activity_finish());

		XposedHelpers.findAndHookMethod(ActivityManagerProxy, "forceStopPackage", String.class, new Hook_ActivityManagerProxy_forceStopPackage());

		XposedHelpers.findAndHookMethod(IntentFilter.class, "match", String.class, String.class, String.class,
			Uri.class, Set.class, String.class, new Hook_IntentFilter_match());
	}

}
