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
        if (intent.getBooleanExtra(Intent.EXTRA_REPLACING, false)) {
            // replacing
        } else if (Intent.ACTION_PACKAGE_REMOVED.equals(action)) {
            UILog.d("action: " + action + ", packageName: " + packageName);
            PreventUtils.update(context, new String[]{packageName}, false);
        } else if (Intent.ACTION_PACKAGE_ADDED.equals(action)) {
            UILog.d("action: " + action + ", packageName: " + packageName);
            PreventUtils.update(context, new String[]{packageName}, true);
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

}
