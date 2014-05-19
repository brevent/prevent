package me.piebridge.forcestopgb;

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
			savePackage(intent.getData().getSchemeSpecificPart(), true);
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

}
