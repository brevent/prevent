package me.piebridge.forcestopgb;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class PackageReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent.getBooleanExtra(Intent.EXTRA_REPLACING, false)) {
            // replacing
        } else if (Intent.ACTION_PACKAGE_REMOVED.equals(intent.getAction())) {
            String packageName = intent.getData().getSchemeSpecificPart();
            context.sendBroadcast(Hook.newIntent(SystemHook.REMOVE_PREVENT_PACKAGE, packageName, null));
        } else if (Intent.ACTION_PACKAGE_ADDED.equals(intent.getAction())) {
            String packageName = intent.getData().getSchemeSpecificPart();
            if (context.getPackageManager().getLaunchIntentForPackage(packageName) != null) {
                context.sendBroadcast(Hook.newIntent(SystemHook.ADD_PREVENT_PACKAGE, packageName, null));
            }
        }
    }

}
