package me.piebridge.prevent.ui;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
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
        } else if (PreventIntent.ACTION_REGISTERED.equals(action)) {
            SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(context);
            long timeout = -1;
            try {
                timeout = Long.parseLong(sp.getString(PreventIntent.KEY_FORCE_STOP_TIMEOUT, "-1"));
            } catch (NumberFormatException e) {
                UILog.d("invalid value for " + PreventIntent.KEY_FORCE_STOP_TIMEOUT, e);
                sp.edit().putString(PreventIntent.KEY_FORCE_STOP_TIMEOUT, "-1").commit();
            }
            boolean destroyProcesses = false;
            try {
                destroyProcesses = sp.getBoolean(PreventIntent.KEY_DESTROY_PROCESSES, false);
            } catch (ClassCastException e) {
                UILog.d("invalid value for " + PreventIntent.KEY_DESTROY_PROCESSES, e);
                sp.edit().putBoolean(PreventIntent.KEY_DESTROY_PROCESSES, false).commit();
            }
            Bundle bundle = new Bundle();
            bundle.putLong(PreventIntent.KEY_FORCE_STOP_TIMEOUT, timeout);
            bundle.putBoolean(PreventIntent.KEY_DESTROY_PROCESSES, destroyProcesses);
            UILog.d("timeout: " + timeout + ", destroyProcesses: " + destroyProcesses);
            PreventUtils.updateConfiguration(context, bundle);
        }
    }

}
