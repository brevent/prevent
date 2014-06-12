package me.piebridge.forcestopgb;

import java.util.HashSet;
import java.util.Set;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;

public class SettingFragmentApplications extends SettingFragment {

	@Override
	protected Set<String> getPackageNames(SettingActivity activity) {
		Set<String> names = new HashSet<String>();
		PackageManager pm = activity.getPackageManager();
		for (PackageInfo pkgInfo : pm.getInstalledPackages(0)) {
			ApplicationInfo appInfo = pkgInfo.applicationInfo;
			if (appInfo == null) {
				continue;
			}
			if (!appInfo.enabled) {
				continue;
			}
			if ((appInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0 && pm.getLaunchIntentForPackage(appInfo.packageName) == null) {
				continue;
			}
			names.add(appInfo.packageName);
		}
		return names;
	}

	@Override
	protected boolean canUseCache() {
		return false;
	}

	private static Position position;

	@Override
	protected void setListPosition(Position _position) {
		position = _position;
	}

	@Override
	protected Position getListPosition() {
		return position;
	}

}