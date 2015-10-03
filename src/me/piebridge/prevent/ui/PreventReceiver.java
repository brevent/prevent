package me.piebridge.prevent.ui;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.preference.PreferenceManager;

import me.piebridge.prevent.common.PackageUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.PreventUtils;

public class PreventReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();
        String packageName = PackageUtils.getPackageName(intent);
        if (intent.getBooleanExtra(Intent.EXTRA_REPLACING, false)) {
            // replacing
        } else if (Intent.ACTION_PACKAGE_REMOVED.equals(action)) {
            UILog.d("action: " + action + ", package: " + packageName);
            PreventUtils.update(context, new String[]{packageName}, false);
        } else if (Intent.ACTION_PACKAGE_ADDED.equals(action)) {
            UILog.d("action: " + action + ", package: " + packageName);
            PreventUtils.update(context, new String[]{packageName}, true);
        } else if (PreventIntent.ACTION_UPDATE_TIMEOUT.equals(action)) {
            String timeout = PreferenceManager.getDefaultSharedPreferences(context).getString(SettingsActivity.KEY_FORCE_STOP_TIMEOUT, "-1");
            UILog.d("timeout: " + timeout);
            try {
                PreventUtils.updateTimeout(context, Long.valueOf(timeout));
            } catch (NumberFormatException e) {
                UILog.d(timeout + " is not long", e);
            }
        }
    }

}
