package me.piebridge.forcestopgb;

import android.app.Activity;
import android.os.Bundle;

public class PreventListFragment extends RefreshableListFragment {
	private MainActivity mActivity;

	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		mActivity = (MainActivity) activity;
	}

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		setListAdapter(new Adapter(mActivity, mActivity.getPackages().keySet()));
	}

	@Override
	public void onDestroyView() {
		super.onDestroyView();
		setListAdapter(null);
	}

	@Override
	public void refresh(boolean force) {
		setListAdapter(null);
		setListAdapter(new Adapter(mActivity, mActivity.getPackages().keySet()));
	}

}