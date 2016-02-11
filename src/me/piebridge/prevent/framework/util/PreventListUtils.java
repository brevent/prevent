package me.piebridge.prevent.framework.util;

import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.res.Resources;
import android.os.Environment;
import android.support.v4.app.NotificationCompat;

import java.io.File;
import java.util.Set;
import java.util.TreeSet;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.FileUtils;
import me.piebridge.prevent.framework.PreventLog;

public final class PreventListUtils {

    public static final String SYSTEM_PREVENT_LIST = "me.piebridge.prevent.list";

    private static PreventListUtils preventListUtils = new PreventListUtils();

    private PreventListUtils() {

    }

    private String getPrevent(Context context) {
        String dataDir;
        try {
            dataDir = context.getPackageManager().getPackageInfo(context.getPackageName(), 0).applicationInfo.dataDir;
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.d("cannot find package for context: " + context, e);
            dataDir = Environment.getDataDirectory() + "/system/";
        }
        File prevent = new File(dataDir, SYSTEM_PREVENT_LIST);
        if (prevent.isDirectory()) {
            FileUtils.eraseFiles(prevent);
        }
        return prevent.getAbsolutePath();
    }

    public synchronized void save(Context context, Set<String> packages, boolean force) {
        String prevent = getPrevent(context);
        if (force || new File(prevent).isFile()) {
            FileUtils.save(getPrevent(context), new TreeSet<String>(packages));
            PreventLog.i("update prevents: " + packages.size());
        }
    }

    public boolean canLoad(Context context) {
        File prevent = new File(getPrevent(context));
        return prevent.isFile() && prevent.canRead();
    }

    public void onRemoved(Context context) {
        File prevent = new File(getPrevent(context));
        if (prevent.isFile()) {
            prevent.delete();
        }
    }

    public Set<String> load(Context context) {
        return FileUtils.load(context, getPrevent(context));
    }

    public static boolean notifyNoPrevents(Context context) {
        ComponentName component = new ComponentName(BuildConfig.APPLICATION_ID, "me.piebridge.prevent.ui.PreventActivity");
        Intent open = new Intent(Intent.ACTION_MAIN);
        open.setComponent(component);
        open.addCategory(Intent.CATEGORY_LAUNCHER);
        open.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        PendingIntent activity = PendingIntent.getActivity(context, 0, open, PendingIntent.FLAG_UPDATE_CURRENT);

        PackageManager pm = context.getPackageManager();
        Resources resources;
        int icon;
        try {
            icon = getNotificationIcon(context);
            resources = pm.getResourcesForApplication(BuildConfig.APPLICATION_ID);
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.e("cannot find " + BuildConfig.APPLICATION_ID, e);
            return false;
        }

        Notification notification = new NotificationCompat.Builder(context)
                .setAutoCancel(true)
                .setShowWhen(false)
                .setContentTitle(resources.getText(R.string.app_name))
                .setContentText(resources.getText(R.string.no_prevents))
                .setTicker(resources.getText(R.string.app_name))
                .setSmallIcon(icon)
                .setContentIntent(activity).build();

        NotificationManager nm = (NotificationManager) context.getSystemService(Context.NOTIFICATION_SERVICE);
        nm.notify(0, notification);
        PreventLog.d("notify using package: " + context.getPackageName());
        return true;
    }

    private static int getNotificationIcon(Context context) throws PackageManager.NameNotFoundException {
        int icon = context.getResources().getIdentifier("ic_menu_blocked_user", "drawable", context.getPackageName());
        if (icon == 0) {
            return context.getPackageManager().getApplicationInfo(context.getPackageName(), 0).icon;
        } else {
            return icon;
        }
    }

    public static PreventListUtils getInstance() {
        return preventListUtils;
    }

}
