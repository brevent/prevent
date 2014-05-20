package me.piebridge.forcestopgb;

import java.util.List;
import java.util.Map;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class PackageReceiver extends BroadcastReceiver {

	@Override
	public void onReceive(Context context, Intent intent) {
		if (Intent.ACTION_PACKAGE_REMOVED.equals(intent.getAction()) && !intent.getBooleanExtra(Intent.EXTRA_REPLACING, false)) {
			savePackage(intent.getData().getSchemeSpecificPart(), false);
		} else if (Intent.ACTION_PACKAGE_ADDED.equals(intent.getAction())) {
			String pkgName = intent.getData().getSchemeSpecificPart();
			if (hasLauncher(context, pkgName)) {
				savePackage(pkgName, true);
			}
		}
	}

	private long mtime = 0;
	protected Map<String, Boolean> packages;

	protected void reloadPackagesIfNeeded() {
		long time = PackageProvider.getMTime(PackageProvider.FORCESTOP);
		if (time > mtime) {
			packages = PackageProvider.loadFromFile(PackageProvider.FORCESTOP);
			mtime = time;
		}
	}

	private void savePackage(String pkgName, boolean added) {
		reloadPackagesIfNeeded();
		if (added && !packages.containsKey(pkgName)) {
			packages.put(pkgName, Boolean.TRUE);
			mtime = PackageProvider.saveToFile(PackageProvider.FORCESTOP, packages, "PackageReceiver");
		} else if (!added && packages.containsKey(pkgName)) {
			packages.remove(pkgName);
			mtime = PackageProvider.saveToFile(PackageProvider.FORCESTOP, packages, "PackageReceiver");
		}
	}

	private boolean hasLauncher(Context context, String pkgName) {
		Intent intent = new Intent(Intent.ACTION_MAIN);
		intent.addCategory(Intent.CATEGORY_LAUNCHER);
		intent.setPackage(pkgName);
		List<?> list = context.getPackageManager().queryIntentActivities(intent, 0);
		if (list != null && list.size() > 0) {
			return true;
		} else {
			return false;
		}
	}
}
