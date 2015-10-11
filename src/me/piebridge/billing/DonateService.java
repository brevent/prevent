package me.piebridge.billing;

import android.content.ComponentName;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.os.RemoteException;

import com.android.vending.billing.IInAppBillingService;

import java.util.Collection;

import me.piebridge.prevent.ui.UILog;

/**
 * Created by thom on 15/10/11.
 */
public class DonateService implements ServiceConnection {

    private IInAppBillingService mService;

    private final DonateActivity mDonateActivity;

    public DonateService(DonateActivity donateActivity) {
        mDonateActivity = donateActivity;
    }

    @Override
    public void onServiceConnected(ComponentName name, IBinder service) {
        mService = IInAppBillingService.Stub.asInterface(service);
        if (!isBillingSupported()) {
            onUnavailable(mService);
        } else if (isDonated()) {
            onDonated(mService);
        } else {
            onAvailable(mService);
        }
        mDonateActivity.unbindService(this);
    }

    protected void onAvailable(IInAppBillingService service) {
        mDonateActivity.onAvailable(service);
    }

    protected void onDonated(IInAppBillingService service) {
        mDonateActivity.onDonated(service);
    }

    protected void onUnavailable(IInAppBillingService service) {
        mDonateActivity.onUnavailable(service);
    }

    @Override
    public void onServiceDisconnected(ComponentName name) {
        mService = null;
    }

    private boolean isBillingSupported() {
        try {
            return mService.isBillingSupported(DonateUtils.API_VERSION, mDonateActivity.getPackageName(), DonateUtils.ITEM_TYPE) == 0;
        } catch (RemoteException e) {
            UILog.d("cannot checking billing supported", e);
            return false;
        }
    }

    protected boolean isDonated() {
        try {
            Bundle bundle = mService.getPurchases(DonateUtils.API_VERSION, mDonateActivity.getPackageName(), DonateUtils.ITEM_TYPE, null);
            return bundle != null && bundle.getInt("RESPONSE_CODE") == 0 && isPurchased(bundle.getStringArrayList("INAPP_DATA_SIGNATURE_LIST"));
        } catch (RemoteException e) {
            UILog.d("cannot get purchases", e);
            return false;
        }
    }

    private boolean isPurchased(Collection<String> signatures) {
        if (signatures == null) {
            return false;
        }
        for (String signature : signatures) {
            if (DonateUtils.isSignature(signature)) {
                return true;
            }
        }
        return false;
    }

}
