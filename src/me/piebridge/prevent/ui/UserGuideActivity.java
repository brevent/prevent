package me.piebridge.prevent.ui;

import android.annotation.SuppressLint;
import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.TypedValue;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.WindowManager;
import android.webkit.WebView;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import me.piebridge.billing.DonateActivity;
import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.EmailUtils;
import me.piebridge.prevent.ui.util.LicenseUtils;
import me.piebridge.prevent.ui.util.QQUtils;
import me.piebridge.prevent.ui.util.RecreateUtils;
import me.piebridge.prevent.ui.util.ThemeUtils;

/**
 * Created by thom on 15/10/3.
 */
public class UserGuideActivity extends DonateActivity implements View.OnClickListener {

    private View donateView;

    private AlertDialog request;

    private ProgressDialog dialog;

    private ProgressDialog donateDialog;

    private BroadcastReceiver receiver;

    private boolean clickedDonate = false;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        ThemeUtils.setTheme(this);
        super.onCreate(savedInstanceState);
        setContentView(R.layout.about);
        ThemeUtils.fixSmartBar(this);

        WebView webView = (WebView) findViewById(R.id.webview);
        webView.setVerticalScrollBarEnabled(false);
        webView.setHorizontalScrollBarEnabled(false);
        if ("zh".equals(Locale.getDefault().getLanguage())) {
            webView.loadUrl("file:///android_asset/about.zh.html");
        } else {
            webView.loadUrl("file:///android_asset/about.en.html");
        }
        ComponentName donateAlipay = getDonateAlipay();
        ComponentName donateWeChat = getDonateWeChat();
        checkView(R.id.alipay, donateAlipay);
        checkView(R.id.wechat, donateWeChat);
        if (setView(R.id.play, "com.android.vending")) {
            findViewById(R.id.play).setVisibility(View.GONE);
            checkDonate();
        }
        donateView = findViewById(R.id.donate);
        if (TextUtils.isEmpty(LicenseUtils.getLicense(this))) {
            donateView.setVisibility(View.VISIBLE);
        } else {
            donateView.setVisibility(View.GONE);
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        checkLicense();
        hideDonateDialog();
    }

    private void checkView(int id, ComponentName component) {
        if (component != null) {
            setView(id, component.getPackageName());
        } else {
            findViewById(id).setVisibility(View.GONE);
        }
    }

    private int getPixel(int dp) {
        return (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dp, getResources().getDisplayMetrics());
    }

    private Drawable cropDrawable(Drawable icon) {
        int width = getPixel(0x20);
        if (icon.getMinimumWidth() > width && icon instanceof BitmapDrawable) {
            Bitmap bitmap = Bitmap.createScaledBitmap(((BitmapDrawable) icon).getBitmap(), width, width, false);
            return new BitmapDrawable(getResources(), bitmap);
        }
        return icon;
    }

    private boolean setView(int id, String packageName) {
        View donate = findViewById(id);
        PackageManager pm = getPackageManager();
        try {
            ApplicationInfo info = pm.getApplicationInfo(packageName, 0);
            if (!info.enabled) {
                donate.setVisibility(View.GONE);
                return false;
            }
            CharSequence label = null;
            if ("com.android.vending".equals(packageName)) {
                Resources resources = pm.getResourcesForApplication(info);
                int appName = resources.getIdentifier("app_name", "string", packageName);
                if (appName > 0) {
                    label = resources.getText(appName);
                }
            }
            if (TextUtils.isEmpty(label)) {
                label = pm.getApplicationLabel(info);
            }

            ImageView image = (ImageView) donate.findViewWithTag("image");
            image.setContentDescription(label);
            image.setImageDrawable(cropDrawable(pm.getApplicationIcon(info)));

            TextView text = (TextView) donate.findViewWithTag("text");
            text.setText(label);

            donate.setClickable(true);
            donate.setOnClickListener(this);
            donate.setVisibility(View.VISIBLE);
            return true;
        } catch (PackageManager.NameNotFoundException e) {
            donate.setVisibility(View.GONE);
            UILog.d("cannot find package " + packageName, e);
            return false;
        }
    }


    private ComponentName getDonateWeChat() {
        return getDonateComponent(PreventIntent.NAME_WECHAT, PreventIntent.CLASS_WECHAT);
    }

    private ComponentName getDonateAlipay() {
        return getDonateComponent(PreventIntent.NAME_ALIPAY, PreventIntent.CLASS_ALIPAY);
    }

    private ComponentName getDonateComponent(String packageName, String className) {
        ComponentName cn = new ComponentName(packageName, className);
        try {
            PackageManager pm = getPackageManager();
            ActivityInfo ai = pm.getActivityInfo(cn, 0);
            int enabled = pm.getComponentEnabledSetting(cn);
            if (BuildConfig.DEBUG) {
                UILog.d("exported: " + ai.exported + ", enabled: " + ai.enabled + ", component enabled: " + enabled);
            }
            if (!ai.exported) {
                return null;
            }
            if (ai.enabled && enabled == PackageManager.COMPONENT_ENABLED_STATE_DEFAULT) {
                return cn;
            }
            if (enabled == PackageManager.COMPONENT_ENABLED_STATE_ENABLED) {
                return cn;
            }
        } catch (PackageManager.NameNotFoundException e) { // NOSONAR
            UILog.d("cannot find " + packageName + "/" + className);
        }
        return null;
    }

    private boolean donateViaWeChat() {
        ComponentName cn = getDonateWeChat();
        if (cn == null) {
            return false;
        }
        showDonateDialog();
        Intent intent = new Intent();
        intent.setComponent(cn);
        intent.putExtra("scene", 1);
        intent.putExtra("receiver_name", BuildConfig.WECHAT_ACCOUNT);
        try {
            startActivity(intent);
        } catch (Throwable t) { // NOSONAR
            hideDonateDialog();
        }
        return true;
    }

    private boolean donateViaAlipay() {
        ComponentName cn = getDonateAlipay();
        if (cn == null) {
            return false;
        }
        showDonateDialog();
        Intent intent = new Intent();
        intent.setComponent(cn);
        intent.putExtra("app_id", "20000053");
        Bundle mExtras = new Bundle();
        mExtras.putString("bizData", BuildConfig.ALIPAY_ACCOUNT);
        intent.putExtra("mExtras", mExtras);
        try {
            startActivity(intent);
        } catch (Throwable t) { // NOSONAR
            hideDonateDialog();
        }
        return true;
    }

    @Override
    public void onClick(View v) {
        int id = v.getId();
        if (id == R.id.wechat) {
            donateViaWeChat();
        } else if (id == R.id.alipay) {
            donateViaAlipay();
        } else if (id == R.id.play) {
            donateViaPlay();
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        menu.clear();
        if (donateView.getVisibility() == View.GONE) {
            menu.add(Menu.NONE, R.string.donate, Menu.NONE, R.string.donate);
        }
        menu.add(Menu.NONE, R.string.version, Menu.NONE, R.string.version);
        menu.add(Menu.NONE, R.string.feedback, Menu.NONE, R.string.feedback);
        menu.add(Menu.NONE, R.string.report_bug, Menu.NONE, R.string.report_bug);
        if (TextUtils.isEmpty(LicenseUtils.getLicense(this))) {
            menu.add(Menu.NONE, R.string.request_license, Menu.NONE, R.string.request_license);
        }
        menu.add(Menu.NONE, R.string.advanced_settings, Menu.NONE, R.string.advanced_settings);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();
        if (id == R.string.donate) {
            clickedDonate = true;
            donateView.setVisibility(View.VISIBLE);
        } else if (id == R.string.feedback) {
            if (!Locale.CHINA.equals(Locale.getDefault()) || !QQUtils.joinQQ(this)) {
                EmailUtils.sendEmail(this, getString(R.string.feedback));
            }
        } else if (id == R.string.report_bug) {
            return requestLog();
        } else if (id == R.string.version) {
            showVersionInfo();
        } else if (id == R.string.advanced_settings) {
            startActivity(new Intent(this, AdvancedSettingsActivity.class));
        } else if (id == R.string.request_license) {
            requestLicense();
        }
        return true;
    }

    private boolean checkLicense() {
        if (LicenseUtils.importLicenseFromClipboard(this)) {
            Toast.makeText(this, R.string.licensed, Toast.LENGTH_LONG).show();
            if (request != null) {
                request.dismiss();
                request = null;
            }
            RecreateUtils.recreate(this);
            return true;
        } else {
            return false;
        }
    }

    private void requestLicense() {
        if (!checkLicense()) {
            Intent intent = new Intent(PreventIntent.ACTION_CHECK_LICENSE, Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
            intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
            sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, new BroadcastReceiver() {
                @Override
                public void onReceive(Context context, Intent intent) {
                    if (PreventIntent.ACTION_CHECK_LICENSE.equals(intent.getAction()) && getResultCode() != 1) {
                        request = LicenseUtils.requestLicense(UserGuideActivity.this, null, getResultData());
                    }
                }
            }, null, 0, null, null);
        }
    }

    private void showProcessDialog(int resId) {
        if (dialog == null) {
            dialog = new ProgressDialog(this);
        }
        dialog.setTitle(R.string.app_name);
        dialog.setIcon(R.drawable.ic_launcher);
        dialog.setCancelable(false);
        dialog.setMessage(getString(resId));
        dialog.show();
    }

    private void showDonateDialog() {
        RelativeLayout layout = new RelativeLayout(this);
        int pixel = getPixel(0x30);
        RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(pixel, pixel);
        params.addRule(RelativeLayout.CENTER_IN_PARENT);
        layout.addView(new ProgressBar(this), params);
        donateDialog = ProgressDialog.show(this, null, null);
        donateDialog.setContentView(layout);
        donateDialog.getWindow().setLayout(WindowManager.LayoutParams.MATCH_PARENT, pixel * 0x4);
    }

    private void hideDonateDialog() {
        if (donateDialog != null) {
            donateDialog.dismiss();
            donateDialog = null;
        }
    }

    private boolean requestLog() {
        File dir = getExternalCacheDir();
        if (dir != null) {
            for (File file : dir.listFiles()) {
                String name = file.getName();
                if (name.startsWith("system.") || name.startsWith("prevent.")) {
                    file.delete();
                }
            }
            Intent intent = new Intent();
            intent.setFlags(Intent.FLAG_RECEIVER_REGISTERED_ONLY | Intent.FLAG_RECEIVER_FOREGROUND);
            intent.setAction(PreventIntent.ACTION_SYSTEM_LOG);
            intent.setData(Uri.fromParts(PreventIntent.SCHEME, getPackageName(), null));
            UILog.i("sending request log broadcast");
            showProcessDialog(R.string.retrieving);
            if (receiver == null) {
                receiver = new HookReceiver();
            }
            sendOrderedBroadcast(intent, PreventIntent.PERMISSION_SYSTEM, receiver, null, 0, null, null);
        }
        return false;
    }

    @Override
    public void onBackPressed() {
        if (clickedDonate && donateView.getVisibility() == View.VISIBLE) {
            donateView.setVisibility(View.GONE);
            clickedDonate = false;
        } else {
            super.onBackPressed();
        }
    }

    @Override
    public void onUnavailable() {
        findViewById(R.id.play).setVisibility(View.GONE);
    }

    @Override
    public void onAvailable() {
        findViewById(R.id.play).setVisibility(View.VISIBLE);
    }

    @Override
    public void onDonated() {
        LicenseUtils.setInAppLicensed();
        invalidateOptionsMenu();
        donateView.setVisibility(View.GONE);
        findViewById(R.id.play).setVisibility(View.GONE);
    }

    private class HookReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();
            if (PreventIntent.ACTION_SYSTEM_LOG.equals(action)) {
                handleRequestLog();
            }
        }

        private void handleRequestLog() {
            runOnUiThread(new Runnable() {
                @Override
                public void run() {
                    dialog.dismiss();
                    reportBug();
                }
            });
        }
    }

    private static Object getXposedVersion() {
        try {
            return Class.forName("de.robv.android.xposed.XposedBridge", false, ClassLoader.getSystemClassLoader()).getField("XPOSED_BRIDGE_VERSION").get(null);
        } catch (Throwable t) { // NOSONAR
            return null;
        }
    }

    private String getVersionInfo(boolean showAppVersion) {
        StringBuilder sb = new StringBuilder();
        String licenseName;
        if (showAppVersion) {
            licenseName = LicenseUtils.getLicense(this);
        } else {
            licenseName = LicenseUtils.getLicenseName(this);
        }
        if (!TextUtils.isEmpty(licenseName)) {
            sb.append(licenseName);
            sb.append("\n");
        }
        sb.append("Xposed: v");
        sb.append(getXposedVersion());
        sb.append("\n");
        sb.append("Android: ");
        sb.append(Locale.getDefault());
        sb.append("-");
        sb.append(Build.VERSION.RELEASE);
        sb.append("\n");
        if (showAppVersion) {
            sb.append(getString(R.string.app_name));
            sb.append(": ");
            sb.append(BuildConfig.VERSION_NAME);
            sb.append("\n");
        }
        sb.append(Build.FINGERPRINT);
        return sb.toString();
    }

    private void showVersionInfo() {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle(getString(R.string.app_name) + "(" + BuildConfig.VERSION_NAME + ")");
        builder.setMessage(getVersionInfo(false));
        builder.setIcon(R.drawable.ic_launcher);
        builder.setPositiveButton(getString(android.R.string.copy), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                ((android.text.ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE)).setText(getVersionInfo(true));
            }
        });
        builder.create().show();
    }

    private void reportBug() {
        try {
            File path = new File(getExternalFilesDir(null), "logs.zip");
            final ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(path));

            for (File file : getExternalCacheDir().listFiles()) {
                zos.putNextEntry(new ZipEntry(file.getName()));
                copyInputStream(zos, file);
            }

            @SuppressLint("SdCardPath")
            File xposedLog = new File("/data/data/de.robv.android.xposed.installer/log/error.log");
            if (xposedLog.isFile() && xposedLog.canRead()) {
                zos.putNextEntry(new ZipEntry("xposed.log"));
                copyInputStream(zos, xposedLog);
            }

            zos.close();
            Runtime.getRuntime().exec("/system/bin/sync");
            EmailUtils.sendZip(this, path, getVersionInfo(true));
        } catch (IOException e) {
            UILog.d("cannot report bug", e);
        }
    }

    private void copyInputStream(ZipOutputStream zos, File file) throws IOException {
        byte[] buffer = new byte[0x1000];
        InputStream is = new FileInputStream(file);
        int length;
        while ((length = is.read(buffer)) > 0) {
            zos.write(buffer, 0, length);
        }
        zos.flush();
        is.close();
    }

}
