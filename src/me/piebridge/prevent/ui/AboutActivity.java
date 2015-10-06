package me.piebridge.prevent.ui;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
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
import android.webkit.WebView;
import android.widget.ImageView;
import android.widget.TextView;

import java.util.Locale;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.forcestopgb.R;
import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.ui.util.LicenseUtils;
import me.piebridge.prevent.ui.util.ThemeUtils;

/**
 * Created by thom on 15/10/3.
 */
public class AboutActivity extends Activity implements View.OnClickListener {

    private View donateView;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        ThemeUtils.setTheme(this);
        super.onCreate(savedInstanceState);
        setContentView(R.layout.about);
        ThemeUtils.fixSmartBar(this);

        WebView webView = (WebView) findViewById(R.id.webview);
        webView.setVerticalScrollBarEnabled(false);
        webView.setHorizontalScrollBarEnabled(false);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP_MR1) {
            setScrollChangeListener(webView);
        }
        if ("zh".equals(Locale.getDefault().getLanguage())) {
            webView.loadUrl("file:///android_asset/about.zh.html");
        } else {
            webView.loadUrl("file:///android_asset/about.en.html");
        }
        checkView(R.id.alipay, getDonateAlipay());
        checkView(R.id.wechat, getDonateWeChat());
        if (!Locale.CHINA.equals(Locale.getDefault())) {
            setView(R.id.paypal, "com.paypal.android.p2pmobile");
        }
        donateView = findViewById(R.id.donate);
        if (TextUtils.isEmpty(LicenseUtils.getLicense(this))) {
            donateView.setVisibility(View.VISIBLE);
        }
    }

    private void checkView(int id, ComponentName component) {
        if (component != null) {
            setView(id, component.getPackageName());
        }
    }

    private int getHeaderIconWidth() {
        return (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, 0x30, getResources().getDisplayMetrics());
    }

    private Drawable cropDrawable(Drawable icon) {
        int width = getHeaderIconWidth();
        if (icon.getMinimumWidth() > width && icon instanceof BitmapDrawable) {
            Bitmap bitmap = Bitmap.createScaledBitmap(((BitmapDrawable) icon).getBitmap(), width, width, false);
            return new BitmapDrawable(getResources(), bitmap);
        }
        return icon;
    }

    private void setView(int id, String packageName) {
        View donate = findViewById(id);
        PackageManager pm = getPackageManager();
        try {
            ApplicationInfo info = pm.getApplicationInfo(packageName, 0);
            CharSequence label = pm.getApplicationLabel(info);

            ImageView image = (ImageView) donate.findViewWithTag("image");
            image.setContentDescription(label);
            image.setImageDrawable(cropDrawable(pm.getApplicationIcon(info)));

            TextView text = (TextView) donate.findViewWithTag("text");
            text.setText(label);

            donate.setClickable(true);
            donate.setOnClickListener(this);
            donate.setVisibility(View.VISIBLE);
        } catch (PackageManager.NameNotFoundException e) {
            UILog.d("cannot find package " + packageName, e);
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
        Intent intent = new Intent();
        intent.setComponent(cn);
        intent.putExtra("scene", 1);
        intent.putExtra("receiver_name", BuildConfig.WECHAT_ACCOUNT);
        try {
            startActivity(intent);
        } catch (Throwable t) { // NOSONAR
            // do nothing
        }
        return true;
    }

    private boolean donateViaAlipay() {
        ComponentName cn = getDonateAlipay();
        if (cn == null) {
            return false;
        }
        Intent intent = new Intent();
        intent.setComponent(cn);
        intent.putExtra("app_id", "20000053");
        Bundle mExtras = new Bundle();
        mExtras.putString("bizData", BuildConfig.ALIPAY_ACCOUNT);
        intent.putExtra("mExtras", mExtras);
        try {
            startActivity(intent);
        } catch (Throwable t) { // NOSONAR
            // do nothing
        }
        return true;
    }

    private boolean donateViaPayPal() {
        try {
            startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(BuildConfig.PAYPAL_ACCOUNT)));
        } catch (Throwable t) { // NOSONAR
            // do nothing
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
        } else if (id == R.id.paypal) {
            donateViaPayPal();
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        menu.clear();
        menu.add(Menu.NONE, R.string.donate, Menu.NONE, R.string.donate);
        menu.add(Menu.NONE, R.string.feedback, Menu.NONE, R.string.feedback);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();
        if (id == R.string.donate) {
            donateView.setVisibility(View.VISIBLE);
        } else if (id == R.string.feedback) {
            EmailUtils.sendEmail(this, getString(R.string.feedback));
        }
        return true;
    }

    private void setScrollChangeListener(final WebView webView) {
        webView.setOnScrollChangeListener(new View.OnScrollChangeListener() {
            @Override
            public void onScrollChange(View v, int scrollX, int scrollY, int oldScrollX, int oldScrollY) {
                if (scrollY > oldScrollY && scrollY >= (webView.getContentHeight() * 0x3ea / 0x400)) {
                    donateView.setVisibility(View.VISIBLE);
                } else if (scrollY < oldScrollY && scrollY == 0) {
                    donateView.setVisibility(View.GONE);
                }
            }
        });
    }

    @Override
    public void onBackPressed() {
        if (donateView.getVisibility() == View.VISIBLE) {
            donateView.setVisibility(View.GONE);
        } else {
            super.onBackPressed();
        }
    }

}
