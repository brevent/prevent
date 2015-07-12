package me.piebridge.forcestopgb.ui;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import me.piebridge.forcestopgb.common.CommonIntent;
import me.piebridge.forcestopgb.hook.SystemHook;
import me.piebridge.forcestopgb.hook.Hook;

public class PackageReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent.getBooleanExtra(Intent.EXTRA_REPLACING, false)) {
            // replacing
        } else if (Intent.ACTION_PACKAGE_REMOVED.equals(intent.getAction())) {
            String packageName = intent.getData().getSchemeSpecificPart();
            PreventUtils.remove(context, new String[]{packageName});
        } else if (Intent.ACTION_PACKAGE_ADDED.equals(intent.getAction())) {
            String packageName = intent.getData().getSchemeSpecificPart();
            PreventUtils.add(context, new String[] {packageName});
        }
    }

}
