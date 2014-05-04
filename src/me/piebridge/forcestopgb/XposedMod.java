package me.piebridge.forcestopgb;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ApplicationInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.os.ParcelFileDescriptor;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XC_MethodHook;
import de.robv.android.xposed.XposedHelpers;

public class XposedMod implements IXposedHookZygoteInit {

	public static final String TAG = "me.piebridge.forcestopgb";
	@SuppressLint("SdCardPath")
	private static final String FORCESTOP = "/data/data/me.piebridge.forcestopgb/conf/forcestop.list";

	abstract class MethodHook extends XC_MethodHook {
		private long mtime;
		protected Set<String> packages;

		protected MethodHook() {
			loadPackages();
		}

		protected void loadPackages() {
			mtime = getMTime();
			packages = loadFromFile(FORCESTOP);
		}

		protected void reloadPackagesIfNeeded() {
			long time = getMTime();
			if (time > mtime) {
				packages = loadFromFile(FORCESTOP);
				mtime = time;
			}
		}

		protected void savePackages(String suffix) {
			saveToFile(FORCESTOP, packages, suffix);
			mtime = getMTime();
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
			String packageName = intent.getComponent().getPackageName();
			reloadPackagesIfNeeded();
			if (packages.contains(packageName)) {
				packages.remove(packageName);
				savePackages("Hook_ActivityManagerProxy_startActivity");
				android.util.Log.d(TAG, "start package " + packageName);
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
			if (!packages.contains(packageName)) {
				packages.add(packageName);
				savePackages("Hook_ActivityManagerProxy_forceStopPackage");
			}
			android.util.Log.d(TAG, "forcestop package " + packageName);
		}
	}

	class Hook_IntentFilter_match extends MethodHook {
		private HashMap<String, Boolean> systemLauncherPackages;;
		public Hook_IntentFilter_match() {
			super();
			systemLauncherPackages = new HashMap<String, Boolean>();
		}

		@Override
		protected void beforeHookedMethod(MethodHookParam param) throws Throwable {
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
			if (isSystemPackage && !systemLauncherPackages.containsKey(packageName)) {
				systemLauncherPackages.put(packageName, Boolean.FALSE);
			}
			String action = (String) param.args[0];
			@SuppressWarnings("unchecked")
			Set<String> categories = (Set<String>) param.args[4];

			if (Intent.ACTION_MAIN.equals(action) && categories != null && categories.contains(Intent.CATEGORY_LAUNCHER)) {
				if (Boolean.FALSE.equals(systemLauncherPackages.get(packageName))) {
					android.util.Log.d(TAG, "system launcher package " + packageName);
					systemLauncherPackages.put(packageName, Boolean.TRUE);
				}
				return;
			}
			reloadPackagesIfNeeded();
			if (packages.contains(packageName) && !Boolean.FALSE.equals(systemLauncherPackages.get(packageName))) {
				param.setResult(IntentFilter.NO_MATCH_ACTION);
				android.util.Log.d(TAG, "ignore intent-filter for: " + packageName + ", action: " + action +
					", categories: " + Arrays.toString(categories == null ? null : categories.toArray(new String[0])));
			}
		}
	}


	@Override
	public void initZygote(IXposedHookZygoteInit.StartupParam startupParam) throws Throwable {
		Class<?> ActivityManagerProxy = Class.forName("android.app.ActivityManagerProxy");
		Class<?> IApplicationThread = Class.forName("android.app.IApplicationThread");

		File parent = new File(FORCESTOP).getParentFile();
		parent.mkdirs();
		setPermissions(parent.getAbsolutePath(), 0777, -1, -1);

		if (Build.VERSION.SDK_INT < 14) {
			XposedHelpers.findAndHookMethod(ActivityManagerProxy, "startActivity", IApplicationThread, Intent.class,
				String.class, Uri[].class, int.class, IBinder.class, String.class,
				int.class, boolean.class, boolean.class, new Hook_ActivityManagerProxy_startActivity(1));
		} else if (Build.VERSION.SDK_INT < 16) {
			XposedHelpers.findAndHookMethod(ActivityManagerProxy, "startActivity", IApplicationThread, Intent.class,
				String.class, Uri[].class, int.class, IBinder.class, String.class, int.class, boolean.class,
				boolean.class, String.class, ParcelFileDescriptor.class, boolean.class, new Hook_ActivityManagerProxy_startActivity(1));
		} else if (Build.VERSION.SDK_INT < 18){
			XposedHelpers.findAndHookMethod(ActivityManagerProxy, "startActivity", IApplicationThread, Intent.class,
				String.class, IBinder.class, String.class, int.class, int.class, String.class,
				ParcelFileDescriptor.class, Bundle.class, new Hook_ActivityManagerProxy_startActivity(1));
		} else {
			XposedHelpers.findAndHookMethod(ActivityManagerProxy, "startActivity", IApplicationThread, String.class, Intent.class,
				String.class, IBinder.class, String.class, int.class,
				int.class, String.class, ParcelFileDescriptor.class, Bundle.class, new Hook_ActivityManagerProxy_startActivity(2));
		}

		XposedHelpers.findAndHookMethod(ActivityManagerProxy, "forceStopPackage", String.class, new Hook_ActivityManagerProxy_forceStopPackage());

		XposedHelpers.findAndHookMethod(IntentFilter.class, "match", String.class, String.class, String.class,
                    Uri.class, Set.class, String.class, new Hook_IntentFilter_match());
	}

	private static void saveToFile(String path, Set<String> packages, String suffix) {
		try {
			File file = new File(path + suffix);
			BufferedWriter writer = new BufferedWriter(new FileWriter(file));
			for (String name : packages) {
				writer.write(name);
				writer.write("\n");
			}
			writer.close();
			setPermissions(file.getAbsolutePath(), 0666, -1, -1);
			file.renameTo(new File(path));
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static Set<String> loadFromFile(String path) {
		HashSet<String> packages = new HashSet<String>();
		try {
			String line;
			File file = new File(path);
			BufferedReader reader = new BufferedReader(new FileReader(file));
			while ((line = reader.readLine()) != null) {
				packages.add(line.trim());
			}
			reader.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return packages;
	}

	private static void setPermissions(String path, int mode, int uid, int gid) {
		try {
			// frameworks/base/core/java/android/os/FileUtils.java
			Class<?> fileUtils = Class.forName("android.os.FileUtils");
			Method setPermissions = fileUtils.getMethod("setPermissions", String.class, int.class, int.class, int.class);
			setPermissions.invoke(null, path, mode, uid, gid);
		} catch (Throwable ex) {
		}
	}

	private static long getMTime() {
		File file = new File(FORCESTOP);
		if (file != null && file.exists()) {
			return file.lastModified();
		}
		return 0L;
	}
}
