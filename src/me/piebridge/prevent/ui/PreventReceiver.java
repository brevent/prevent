package me.piebridge.prevent.ui;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v4.app.NotificationCompat;

import java.io.File;
import java.util.ArrayList;
import java.util.Set;

import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.PackageUtils;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.PreventListUtils;
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
            updateConfiguration(context, true);
        } else if (PreventIntent.ACTION_NOT_SUPPORTED.equals(action)) {
            notifyNotSupported(context);
        }
    }

    public static void updateConfiguration(Context context) {
        updateConfiguration(context, false);
    }

    private static void updateConfiguration(Context context, boolean updatePreventList) {
        SharedPreferences sp = PreferenceManager.getDefaultSharedPreferences(context);
        long timeout = -1;
        try {
            timeout = Long.parseLong(sp.getString(PreventIntent.KEY_FORCE_STOP_TIMEOUT, "-1"));
        } catch (NumberFormatException e) {
            UILog.d("invalid value for " + PreventIntent.KEY_FORCE_STOP_TIMEOUT, e);
            sp.edit().putString(PreventIntent.KEY_FORCE_STOP_TIMEOUT, "-1").apply();
        }
        boolean destroyProcesses = getPreference(sp, PreventIntent.KEY_DESTROY_PROCESSES, false);
        boolean lockSyncSettings = getPreference(sp, PreventIntent.KEY_LOCK_SYNC_SETTINGS, false);
        boolean useAppStandby = getPreference(sp, PreventIntent.KEY_USE_APP_STANDBY, false);
        Bundle bundle = new Bundle();
        bundle.putLong(PreventIntent.KEY_FORCE_STOP_TIMEOUT, timeout);
        bundle.putBoolean(PreventIntent.KEY_DESTROY_PROCESSES, destroyProcesses);
        bundle.putBoolean(PreventIntent.KEY_LOCK_SYNC_SETTINGS, lockSyncSettings);
        bundle.putBoolean(PreventIntent.KEY_USE_APP_STANDBY, useAppStandby);
        UILog.d("timeout: " + timeout + ", destroyProcesses: " + destroyProcesses
                + ", lockSyncSettings: " + lockSyncSettings + ", useAppStandby: " + useAppStandby);
        if (updatePreventList) {
            eraseFiles(context.getExternalCacheDir());
            Set<String> prevents = PreventListUtils.load(context);
            if (prevents.isEmpty()) {
                notifyNoPrevents(context);
            }
            bundle.putStringArrayList(PreventIntent.KEY_PREVENT_LIST, new ArrayList<String>(prevents));
        }
        PreventUtils.updateConfiguration(context, bundle);
    }

    private static boolean getPreference(SharedPreferences sp, String key, boolean defaultValue) {
        boolean value = defaultValue;
        try {
            value = sp.getBoolean(key, defaultValue);
        } catch (ClassCastException e) {
            UILog.d("invalid value for " + key, e);
            sp.edit().putBoolean(key, defaultValue).apply();
        }
        return value;
    }

    private static void notifyNoPrevents(Context context) {
        Intent open = new Intent(context, PreventActivity.class);
        open.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        PendingIntent activity = PendingIntent.getActivity(context, 0, open, PendingIntent.FLAG_UPDATE_CURRENT);

        Notification notification = new NotificationCompat.Builder(context)
                .setAutoCancel(true)
                .setContentTitle(context.getText(R.string.app_name))
                .setContentText(context.getText(R.string.no_prevents))
                .setTicker(context.getText(R.string.app_name))
                .setSmallIcon(R.drawable.ic_launcher)
                .setContentIntent(activity).build();

        NotificationManager nm = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        nm.notify(0, notification);
    }

    private static boolean eraseFiles(File path) {
        if (path == null) {
            return false;
        }
        if (path.isDirectory()) {
            String[] files = path.list();
            if (files != null) {
                for (String file : files) {
                    eraseFiles(new File(path, file));
                }
            }
        }
        return path.delete();
    }

    private static void notifyNotSupported(Context context) {
        Intent open = new Intent(context, PreventActivity.class);
        open.setAction(PreventIntent.ACTION_NOT_SUPPORTED);
        open.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        PendingIntent activity = PendingIntent.getActivity(context, 0, open, PendingIntent.FLAG_UPDATE_CURRENT);

        Notification notification = new NotificationCompat.Builder(context)
                .setAutoCancel(false)
                .setContentTitle(context.getText(R.string.app_name))
                .setContentText(context.getText(R.string.not_supported))
                .setTicker(context.getText(R.string.app_name))
                .setSmallIcon(R.drawable.ic_launcher)
                .setContentIntent(activity).build();

        NotificationManager nm = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        nm.notify(0, notification);
    }
}
