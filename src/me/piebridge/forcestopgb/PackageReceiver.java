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
			String pkgName = intent.getData().getSchemeSpecificPart();
			if (context.getPackageManager().getLaunchIntentForPackage(pkgName) != null) {
				savePackage(pkgName, true);
			}
		}
	}

	private void savePackage(String pkgName, boolean added) {
		Map<String, Boolean> packages = PreventPackages.load();
		if (added && !packages.containsKey(pkgName)) {
			packages.put(pkgName, Boolean.TRUE);
			PreventPackages.save(packages, "PackageReceiver");
		} else if (!added && packages.containsKey(pkgName)) {
			packages.remove(pkgName);
			PreventPackages.save(packages, "PackageReceiver");
		}
	}
}
