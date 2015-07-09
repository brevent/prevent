package me.piebridge.forcestopgb;

import java.util.Map;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;

public class PackageReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent.getBooleanExtra(Intent.EXTRA_REPLACING, false)) {
            // replacing
        } else if (Intent.ACTION_PACKAGE_REMOVED.equals(intent.getAction())) {
            savePackage(context, intent.getData().getSchemeSpecificPart(), false);
        } else if (Intent.ACTION_PACKAGE_ADDED.equals(intent.getAction())) {
            String pkgName = intent.getData().getSchemeSpecificPart();
            if (context.getPackageManager().getLaunchIntentForPackage(pkgName) != null) {
                savePackage(context, pkgName, true);
            }
        }
    }

    private void savePackage(Context context, String pkgName, boolean added) {
        context.sendBroadcast(new Intent(SystemHook.ACTION_SAVE_PACKAGE, Uri.fromParts("package", pkgName, String.valueOf(added))));
    }
}
