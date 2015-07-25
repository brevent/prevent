package me.piebridge.prevent.ui;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

import java.util.Set;

import me.piebridge.prevent.ui.util.PreventListUtils;
import me.piebridge.prevent.ui.util.PreventUtils;

public class PreventReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();
        String packageName = getPackageName(intent);
        UILog.d("action: " + action + ", packageName: " + packageName);
        if (intent.getBooleanExtra(Intent.EXTRA_REPLACING, false)) {
            // replacing
        } else if (Intent.ACTION_PACKAGE_REMOVED.equals(action)) {
            updatePrevents(context, packageName, false);
        } else if (Intent.ACTION_PACKAGE_ADDED.equals(action)) {
            updatePrevents(context, packageName, true);
        }
    }

    private String getPackageName(Intent intent) {
        if (intent == null) {
            return null;
        }
        Uri uri = intent.getData();
        if (uri == null) {
            return null;
        }
        return uri.getSchemeSpecificPart();
    }

    private static void updatePrevents(Context context, String packageName, boolean added) {
        PreventUtils.update(context, new String[]{packageName}, added);
        Set<String> packages = PreventListUtils.load();
        if (added && packages.add(packageName)) {
            PreventListUtils.save(packages);
        } else if (!added && packages.remove(packageName)) {
            PreventListUtils.save(packages);
        }
    }

}
