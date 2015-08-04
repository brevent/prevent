package me.piebridge.prevent.framework;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import me.piebridge.prevent.common.PackageUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.WidgetUtils;

/**
 * Created by thom on 15/8/4.
 */
public class PackageReceiver extends BroadcastReceiver {
    @Override
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();
        String packageName = PackageUtils.getPackageName(intent);
        if (Intent.ACTION_PACKAGE_RESTARTED.equals(action)) {
            handlePackageRestarted("PACKAGE_RESTARTED", packageName);
        } else if (Intent.ACTION_PACKAGE_ADDED.equals(action)) {
            WidgetUtils.cleanWidgets();
        }
    }

    private void handlePackageRestarted(String action, String packageName) {
        LogUtils.logRequest(action, packageName, -1);
        SystemHook.getPackageCounters().remove(packageName);
        if (SystemHook.getPreventPackages().containsKey(packageName)) {
            SystemHook.getPreventPackages().put(packageName, Boolean.TRUE);
        }
        SystemHook.killNoFather();
    }

}
