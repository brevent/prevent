package me.piebridge.forcestopgb.ui;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;

import java.util.HashSet;
import java.util.Set;

import me.piebridge.forcestopgb.R;
import me.piebridge.util.PackageUtils;

public class SettingFragmentPreventList extends SettingFragment {

    @Override
    protected Set<String> getPackageNames(SettingActivity activity) {
        Set<String> names = new HashSet<String>();
        PackageManager pm = activity.getPackageManager();
        Set<String> removes = new HashSet<String>();
        for (String packageName : activity.getPreventPackages().keySet()) {
            ApplicationInfo appInfo;
            try {
                appInfo = pm.getApplicationInfo(packageName, 0);
            } catch (PackageManager.NameNotFoundException e) { // NOSONAR
                appInfo = null;
            }
            if (appInfo == null || !appInfo.enabled || (PackageUtils.isSystemPackage(appInfo.flags) && pm.getLaunchIntentForPackage(packageName) == null)) {
                removes.add(packageName);
            } else {
                names.add(packageName);
            }
        }
        if (!removes.isEmpty()) {
            PreventUtils.remove(getActivity(), removes.toArray(new String[removes.size()]));
        }
        return names;
    }

    @Override
    protected boolean canUseCache() {
        return true;
    }

    @Override
    protected int getQueryHint() {
        return R.string.query_hint_system;
    }

}